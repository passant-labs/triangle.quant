package mage.triangle.exchanges

import mage.triangle.entity.{Amount, Balance, Buy, Code, CodeOrSym, Currency, Depth, ID, Instrument, Order, OrderID, Price, Quantity, Quote, Ratio, Sell, Sym}
import extension.concurrent.FutureOps
import extension.data.JSON
import extension.data.JSON._
import extension.system.{halt, StringConversation}
import extension.io.{HTTP, String2URI}
import extension.io.WebSocket.JValueDecodeWithGZIP
import extension.logging.ILogger
import extension.math.{String2Decimal, TEN, ZERO}
import mage.triangle.entity.{Balance, Depth, Order}

import java.net.{URI, URLEncoder}
import java.time.{ZonedDateTime, ZoneId}
import java.time.format.DateTimeFormatter
import java.util.Base64
import javax.crypto.spec.SecretKeySpec
import javax.crypto.Mac
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Future, Promise}
class Huobi extends Exchange with ILogger {

  /**
    *  {
    *    "id": 59378,
    *    "symbol": "ethusdt",
    *    "account-id": 100009,
    *    "amount": "10.1000000000",
    *    "price": "100.1000000000",
    *    "created-at": 1494901162595,
    *    "type": "buy-limit",
    *    "field-amount": "10.1000000000",
    *    "field-cash-amount": "1011.0100000000",
    *    "field-fees": "0.0202000000",
    *    "finished-at": 1494901400468,
    *    "user-id": 1000,
    *    "source": "api",
    *    "state": "filled",
    *    "canceled-at": 0
    *  }
    */
  case class ExOrder(
      id:              ID,
      symbol:          Sym,
      amount:          Quantity,
      price:           Price,
      fieldAmount:     Quantity,
      fieldCashAmount: Amount,
      createdAt:       Long
  ) {
    def order: Order = ???
  }

  object Sign {
    private final val ApiKey: String = s"$exName.api.key".prop
    private final lazy val ApiSecret: SecretKeySpec =
      new SecretKeySpec(s"$exName.api.secret".prop.getBytes, "HmacSHA256")

    val baseUrl:   String            = "https://api-aws.huobi.pro/v1"
    val wsUrl:     URI               = "wss://api-aws.huobi.pro/ws/v2"
    val base64:    Base64.Encoder    = Base64.getEncoder
    val zone:      ZoneId            = ZoneId.of("UTC")
    val dateFmt:   DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH'%3A'mm'%3A'ss")
    val wsDateFmt: DateTimeFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss")

    final val defaultHeaders = List(
      "Content-Type" -> "application/json; charset=UTF-8"
    )
    def base64digest(s: String): String = {
      val mac = Mac.getInstance("HmacSHA256")
      mac.init(ApiSecret)
      val bs = mac.doFinal(s.getBytes())
      base64.encodeToString(bs)
    }

    def apply(method: String, path: String, body: String = null): (String, URI, List[(String, String)]) = {
      val dateStr = ZonedDateTime.now(zone).format(dateFmt)
      val uri: URI = baseUrl + path
      var qs = s"AccessKeyId=$ApiKey&SignatureMethod=HmacSHA256&SignatureVersion=2&Timestamp=$dateStr"
      if (uri.getQuery != null) {
        qs += s"&${uri.getQuery}"
      }
      val host     = uri.getHost
      val pth      = uri.getPath
      val s        = s"$method\n$host\n$pth\n$qs"
      val sig      = URLEncoder.encode(base64digest(s), "UTF-8")
      val finalUri = s"${uri.getScheme}://$host$pth?$qs&Signature=$sig"
      (method, finalUri, defaultHeaders)
    }

    def wsSign(): Map[String, Any] = {
      val method = "GET"
      val params = List(
        "accessKey"        -> ApiKey,
        "signatureMethod"  -> "HmacSHA256",
        "signatureVersion" -> "2.1",
        "timestamp"        -> ZonedDateTime.now(zone).format(wsDateFmt)
      )
      val qs = params.map { case (k, v) => s"$k=${URLEncoder.encode(v, "UTF-8")}" }.mkString("&")

      val s   = s"$method\n${wsUrl.getHost}\n${wsUrl.getPath}\n$qs"
      val sig = base64digest(s)

      val payload = params.toMap ++ Map("authType" -> "api", "signature" -> sig)

      Map("action" -> "req", "ch" -> "auth", "params" -> payload)

    }
  }

  val takerCommRate: Ratio = "0.0005".d
  val makerCommRate: Ratio = "-0.00002".d

  lazy val accountID: Long = getSpotAccountID()

  lazy val instrs: Map[Code, Instrument] = {
    val str = HTTP
      .request(uri = "https://api-aws.huobi.pro/v1/common/symbols")
      .await()
      .body()
      .string()

    val rsp    = JSON.parse(str)
    var instrs = Map[CodeOrSym, Instrument]()
    for (i <- (rsp \ "data").children) {
      val sym          = (i \ "symbol").as[Sym]
      val base         = (i \ "base-currency").as[Currency].toUpperCase
      val quote        = (i \ "quote-currency").as[Currency].toUpperCase
      val pxPrecision  = (i \ "price-precision").as[Int]
      val qtyPrecision = (i \ "amount-precision").as[Int]
      val minOrdQty    = (i \ "min-order-amt").as[Quantity]
      val minOrdValue  = (i \ "min-order-value").as[Amount]

      val code = base + ':' + quote
      val tick = TEN.pow(-pxPrecision.toInt)
      val lot  = minOrdQty.max(TEN.pow(-qtyPrecision.toInt))
      val instr = Instrument(
        code          = code,
        exName        = this.exName,
        sym           = sym,
        base          = base,
        quote         = quote,
        tick          = tick,
        lot           = lot,
        minOrdValue   = minOrdValue,
        takerCommRate = takerCommRate,
        makerCommRate = makerCommRate
      )

      instrs += (instr.code -> instr)
      instrs += (instr.sym  -> instr)

    }
    instrs
  }

  protected lazy final val ws =
    new WebSocket[JValue]("wss://api-aws.huobi.pro/ws") {
      override def topicOf(v: JValue): String = {
        val ch = (v \ "ch").getAs[String]
        if (ch.isDefined) return ch.get
        val ping = (v \ "ping").getAs[Long]
        if (ping.isDefined) return "ping"
        null
      }

      override def onError(e: Throwable): Unit = {
        try {
          super.onError(e)
        } catch {
          case e: Throwable =>
            log.error(e)("unknown error")
            halt(1)
        }
      }
      this.subscribe(null) { msg => log.warn(JSON.write(msg)) }
      this.subscribe("ping") { msg =>
        send(Map("pong" -> (msg \ "ping").as[Long]))
      }
    }

  protected lazy final val myWs = {
    val ws = new WebSocket[JValue](Sign.wsUrl) {
      override def topicOf(v: JValue): String = {
        val action = (v \ "action").getAs[String].orNull
        action match {
          case "ping" => "ping"

          case "req" =>
            if ((v \ "ch").as[String] == "auth" && (v \ "code").as[Int] != 200) {
              throw AuthenticationFailure(exName, s"auth failed: ${Sign.wsUrl}")
            }
            null

          case "push" =>
            val ch = (v \ "ch").as[String]
            if (ch.startsWith("orders")) "orders"
            else if (ch.startsWith("accounts")) "accounts"
            else null

          case _ => null
        }
      }

      this.subscribe(null) { msg => log.warn(JSON.write(msg)) }
      this.subscribe("ping") { msg =>
        send(
          Map(
            "action" -> "pong",
            "data"   -> Map("ts" -> (msg \ "data" \ "ts").as[Long])
          )
        )
      }
    }

    ws.onConnected("auth") {
      ws.send(Sign.wsSign())
      Thread.sleep(500)
    }

    ws
  }

  private def getSpotAccountID(): Long = {
    val (mthd, uri, hds) = Sign("GET", "/account/accounts")
    val rsp              = HTTP.request(mthd, uri.toString, hds).await()
    val body             = JSON.parse(rsp.body().byteStream())
    val acct = (body \ "data").children.find { acct =>
      (acct \ "type").as[String] == "spot"
    }.get
    (acct \ "id").as[Long]
  }

  def placeOrder(ord: Order): Future[OrderID] = {
    val typ = if (ord.postOnly) {
      if (ord.side.isBuy) "buy-limit-maker" else "sell-limit-maker"
    } else {
      (ord.side, ord.isLimit, ord.tif) match {
        case (Buy, true, Order.TimeInForce.GTC)  => "buy-limit"
        case (Sell, true, Order.TimeInForce.GTC) => "sell-limit"

        case (Buy, true, Order.TimeInForce.IOC)  => "buy-ioc"
        case (Sell, true, Order.TimeInForce.IOC) => "sell-ioc"

        case (Buy, false, Order.TimeInForce.GTC)  => "buy-market"
        case (Sell, false, Order.TimeInForce.GTC) => "sell-market"
      }
    }
    val data = Map(
      "account-id"      -> accountID,
      "symbol"          -> ord.sym,
      "type"            -> typ,
      "amount"          -> ord.qty,
      "client-order-id" -> ord.clID,
      "price"           -> ord.px
    )
    val body                 = JSON.write(data)
    val (mthd, uri, headers) = Sign("POST", "/order/orders/place", body)
    HTTP.request(mthd, uri.toString, headers, body) map { rsp =>
      val body = rsp.body().string()
      if (body.contains("account-frozen-balance-insufficient-error")) {
        throw new InsufficientAsset(exName, body) with OrderRejected
      } else if (body.contains("order-limitorder-amount-min-error")) {
        throw new MinLotLimited(exName, body) with OrderRejected
      } else if (body.contains("invalid-client-order-id")) {
        throw new BadlyFormed(exName, body) with OrderRejected
      } else if (body.contains("error")) {
        log.error(s"placeOrder: $ord $body")
        "-1"
      } else {
        (JSON.parse(body) \ "data").as[String]
      }
    }
  }

  def cancelOrder(fxID: OrderID, clID: ID): Unit = {
    checkIDS(fxID, clID)
    val (path, data) = if (fxID != null) {
      (s"/order/orders/$fxID/submitcancel", "{}")
    } else {
      (s"/order/orders/submitCancelClientOrder", JSON.write(Map("client-order-id" -> clID)))
    }

    val (mthd, uri, headers) = Sign("POST", path, data)
    HTTP.request(mthd, uri.toString, headers, data) map { rsp =>
      val body = rsp.body().string()
      if (body.contains("error")) {
        log.error(s"cancelOrder: $fxID | $clID $body")
      }
    }
  }

  def fetchOrder(fxID: OrderID, clID: ID): Future[Order] = {
    checkIDS(fxID, clID)
    val path = if (fxID != null) {
      s"/order/orders/$fxID"
    } else {
      s"/order/orders/getClientOrder?clientOrderId=$clID"
    }

    val (mthd, uri, headers) = Sign("GET", path)

    HTTP.request(mthd, uri.toString, headers) map { rsp =>
      val json = JSON.parse(rsp.body().byteStream())

      if ((json \ "status").as[String] == "error") {
        (json \ "err-code").as[String] match {
          case "base-record-invalid" => throw OrderNonexistent(exName, s"$fxID | $clID")
          case _                     => throw Unknown(exName, JSON.write(json))
        }
      } else {
        val o = json \ "data"

        val sym   = (o \ "symbol").as[String]
        val instr = instrs(sym)

        val status = (o \ "state").getAsOrElse[String](null) match {
          case "filled" | "canceled" | "partial-canceled" => Order.Status.Closed
          case _                                          => Order.Status.New
        }

        Order(
          exName   = exName,
          code     = instr.code,
          fxID     = (o \ "id").as[String],
          clID     = (o \ "client-order-id").getAsOrElse[String]("0").toLong,
          side     = if ((o \ "type").as[String].startsWith("buy")) Buy else Sell,
          px       = (o \ "price").as[Price],
          qty      = (o \ "amount").as[Quantity],
          cumQty   = (o \ "field-amount").as[Quantity],
          cumValue = (o \ "field-cash-amount").as[Amount],
          status   = status,
          txTs     = (o \ "created-at").as[Long]
        )
      }
    }
  }

  def fetchOpeningOrders(): Future[List[Order]] = {
    val path                 = s"/order/openOrders?account-id=$accountID"
    val (mthd, uri, headers) = Sign("GET", path)
    null
  }

  def subscribeDepth(sym: Sym, depth: Depth)(cb: Depth => Unit): Unit = {
    val instr = instrs(sym)
    val topic = s"market.${instr.sym}.bbo"
    val data  = Map("sub" -> topic)
    ws.onConnected(topic) { ws.send(data) }
    ws.subscribe(topic) { msg =>
      val data   = msg \ "tick"
      val bidPx  = (data \ "bid").as[BigDecimal]
      val bidQty = (data \ "bidSize").as[BigDecimal]
      val askPx  = (data \ "ask").as[BigDecimal]
      val askQty = (data \ "askSize").as[BigDecimal]
      val bid    = Quote(instr, Buy, bidPx, bidQty)
      val ask    = Quote(instr, Sell, askPx, askQty)
      depth.clear()
      depth.put(bid)
      depth.put(ask)
      depth.commit()
      cb(depth)
    }
  }

  private def handleOrderMsg(cb: Order => Unit, msg: JValue): Unit = {
    val exOrd    = msg \ "data"
    val sym      = (exOrd \ "symbol").as[String]
    val instr    = instrs(sym)
    val px       = (exOrd \ "orderPrice").as[Price]
    val cumQty   = (exOrd \ "execAmt").getAsOrElse[Quantity](ZERO)
    val cumValue = cumQty * px
    val status = (exOrd \ "orderStatus").getAsOrElse[String](null) match {
      case "filled" | "canceled" | "partial-canceled" => Order.Status.Closed
      case _                                          => Order.Status.New
    }
    val txTs  = (exOrd \ "orderCreateTime").getAsOrElse[Long](0)
    val sClID = (exOrd \ "clientOrderId").getAsOrElse[String]("")
    if (sClID.isEmpty) return
    val clID = sClID.toLong
    val ord = Order(
      exName   = exName,
      code     = instr.code,
      fxID     = (exOrd \ "orderId").as[String],
      clID     = clID,
      side     = if ((exOrd \ "type").as[String].startsWith("buy")) Buy else Sell,
      px       = px,
      qty      = (exOrd \ "orderSize").as[Quantity],
      cumQty   = cumQty,
      cumValue = cumValue,
      status   = status,
      txTs     = txTs
    )
    cb(ord)
  }

  def subscribeOrders(cb: Order => Unit): Unit = {
    val topic = "orders"
    val ws    = myWs
    ws.onConnected(topic) {
      ws.send(Map("action" -> "sub", "ch" -> "orders#*"))
    }
    ws.subscribe(topic)(handleOrderMsg(cb, _))
  }

  def fetchBalances(): Future[List[Balance]] = ???

  def subscriberBalances(cb: Balance => Unit): Unit = {
    val topic = "accounts"
    val ws    = myWs
    ws.onConnected(topic) {
      ws.send(Map("action" -> "sub", "ch" -> "accounts.update#0"))
    }
    ws.subscribe(topic) { data =>
      val d = data \ "data"
      if ((d \ "accountType").as[String] == "trade") {
        val bls = Balance(
          curr   = (d \ "currency").as[String].toUpperCase,
          amount = (d \ "balance").as[BigDecimal]
        )
        cb(bls)
      }
    }
  }
}

object Huobi {
  def main(args: Array[String]): Unit = {
    val ex = new Huobi()
    ex.instrs
//    println(ex.instrs)
    println(ex.accountID)
    val ord = Order(
      exName = ex.exName,
      code   = "BTC:USDT",
      side   = Sell,
      px     = "50000".d,
      qty    = "0.01".d
    )
    ex.subscribeOrders { ord =>
      println(s"~~~~~~~~~ $ord")
    }
//    println(ex.fetchOrder(clID = 1613026075345001L).await())
    println(ex.fetchOrder(fxID = "204450349355107").await())
//    ex.placeOrder(ord)
//    Thread.sleep(3000)
//    ex.cancelOrder(ord)
  }
}
