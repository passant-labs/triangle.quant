package mage.triangle

import mage.triangle.entity._
import mage.triangle.exchanges.OrderRejected
import com.redis.RedisClient
import com.redis.api.StringApi.NX
import extension.collection.ToMap
import extension.concurrent.{schedule, thread}
import extension.data.{CSV, JSON}
import extension.io.String2URI
import extension.logging.{ILogger, Log}
import extension.math.{BigDecimalOps, String2Decimal}
import extension.system.{halt, loadProps, StringConversation}
import mage.triangle.entity.Order.{Status, TimeInForce, Type}
import mage.triangle.entity.{Balance, Buy, Instrument, Order, Sell, Side}
import mage.triangle.exchanges.{Exchange, OrderNonexistent, OrderRejected}
import org.json4s.{CustomSerializer, JString}

import java.io.File
import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global
import scala.math.BigDecimal.RoundingMode.DOWN
import scala.util.{Failure, Success}

class Arb(
    val targetCurr: Currency,
    val bridgeCurr: Currency,
    val exName:     ExName,
    val aInstrs:    Map[Currency, Instrument],
    val bInstrs:    Map[Currency, Instrument],
    val cInstr:     Instrument
) extends ILogger {

  val altCurrs: Set[Currency] = aInstrs.keySet.intersect(bInstrs.keySet)

  val pricings: Map[Currency, Pricing] =
    altCurrs.xMap(altCurr => altCurr -> new Pricing(aInstrs(altCurr), bInstrs(altCurr), cInstr))

  val pricing: Pricing = pricings.head._2

  val exchange: Exchange = Exchange(exName)

  val redisUrl: String = "redis.url".prop

  lazy val redis = new RedisClient(redisUrl)

  lazy val storage = new RedisStorage(redisUrl)

  lazy val cache = new PersistentCache(cInstr, storage)

  private def openingAOrders(instr: Instrument) = {
    if (instr.eq(cInstr)) {
      cache.getOrders.filter(o => aInstrs.contains(o.instr.base))
    } else {
      val curr   = instr.base
      val aInstr = aInstrs(curr)
      cache.getOrders.filter(_.code == aInstr.code)
    }
  }

  def checkAOrders(instr: Instrument): Unit = {
    val openAOrds = openingAOrders(instr)
    val brokenOrds = openAOrds.filter { ord =>
      val pricing = pricings(ord.instr.base)
      pricing.broken(ord.px)
    }

    brokenOrds.foreach(cancelOrder)
  }

  def nonCanceling(ord: Order): Boolean = {
    try {
      redis.set(ord.id, "0", NX, 1.seconds)
    } catch {
      case e: Throwable =>
        log.error(s"canceling ${e.getMessage}")
        true
    }
  }

  def cancelOrder(ord: Order): Unit = {
    if (nonCanceling(ord)) exchange.cancelOrder(ord)
  }

  def placeOrder(ord: Order): Unit = {
    exchange.placeOrder(ord) onComplete {
      case Success(_) =>
      case Failure(e: OrderRejected) =>
        log.error(s"$ord: ${e.getMessage}")
        cache.delete(ord)
      case _ =>
    }
  }

  def placeAOrder(aInstr: Instrument): Unit = {
    val pricing = pricings(aInstr.base)
    val px      = pricing.apx()
    if (pricing.broken(px)) return
    val ord = entity.Order(
      exName   = exName,
      code     = aInstr.code,
      side     = Buy,
      px       = px,
      qty      = aInstr.maxOrdQty,
      execInst = Order.ExecInst.PostOnly
    )
    log.info(s"ArbOrder: $ord")
    cache.insert(ord)
    placeOrder(ord)
  }

  def placeAOrders(instr: Instrument): Unit = {
    val ords = openingAOrders(instr).asMap(_.instr.base)
    if (instr == cInstr) {
      aInstrs.values.filterNot(i => ords.contains(i.base)).foreach(placeAOrder)
    } else if (ords.isEmpty) {
      placeAOrder(aInstrs(instr.base))
    }
  }

  object HedgeLoop {

    val balances: mutable.Map[Currency, Balance] = mutable.Map()

    def start(): Unit = {
      thread {
        while (true) {
          synchronized { wait(1000) }
          val orders = cache.getOrders.toList
          orders.foreach(checkOrder)
          val hedging = orders.asMap { ord =>
            if (ord.side.isBuy) ord.instr.quote else ord.instr.base
          }
          balances.keys.foreach { curr =>
            if (!hedging.contains(curr) && (curr == bridgeCurr || aInstrs.contains(curr))) {
              hedge(curr)
            }
          }
        }
      }
    }

    def push(bls: Balance): Unit = synchronized {
      balances.put(bls.curr, bls)
      notify()
    }

    def !!!(): Unit = synchronized {
      notify()
    }

    def checkOrder(ord: Order): Unit = {
      val p = if (ord.instr == cInstr) {
        pricing
      } else {
        val curr = ord.instr.base
        if (aInstrs.contains(curr)) return
        pricings(curr)
      }
      if (p.brokenSpread(ord)) cancelOrder(ord)
    }

    def hedge(curr: Currency): Unit = {
      if (curr == targetCurr) return
      val amt = synchronized { balances(curr).amount }
      val (side, instr, px) =
        if (curr == bridgeCurr)
          (Buy, cInstr, pricing.cMakerPx())
        else
          (Sell, bInstrs(curr), pricings(curr).bMakerPx())

      val qty = (if (side.isSell) amt else amt / px).roundWith(instr.lot, DOWN)

      if (qty < instr.lot || qty * px < instr.minOrdValue) return
      val ord = entity.Order(
        exName   = exName,
        code     = instr.code,
        side     = side,
        px       = px,
        qty      = qty,
        execInst = Order.ExecInst.PostOnly
      )
      cache.insert(ord)
      placeOrder(ord)
    }
  }

  object ArbLoop {

    @volatile var instrs: Set[Instrument] = Set[Instrument]()

    final val lostCounts = mutable.Map[ID, Int]()

    def start(): Unit = {
      thread {
        while (true) {
          val is = synchronized {
            wait(1000)
            val is = instrs
            instrs = is.empty
            is
          }
          is.foreach { instr =>
            checkAOrders(instr)
            placeAOrders(instr)
          }
        }
      }

      schedule(5000, 5000) {
        cache.getOrders.foreach { o =>
          exchange.fetchOrder(fxID = o.fxID, clID = o.clID) onComplete {
            case Success(ord) =>
              lostCounts.synchronized {
                cache.update(ord)
                lostCounts -= o.id
              }
            case Failure(e: OrderNonexistent) =>
              lostCounts.synchronized {
                if (lostCounts.contains(o.id)) {
                  log.error(s"order not exists: $o")
                  cache.delete(o)
                  lostCounts -= o.id
                } else {
                  lostCounts += o.id -> 1
                }
              }
            case Failure(e) =>
              log.error(e)("Unknown")
          }
        }
      }
    }

    def push(i: Instrument): Unit = synchronized {
      instrs += i
      notify()
    }
  }

  def start(): Unit = {

    cache.load()
    ArbLoop.start()
    HedgeLoop.start()

    (aInstrs.values ++ bInstrs.values ++ List(cInstr)).foreach { instr =>
      exchange.subscribeDepth(code = instr.code) { depth =>
        ArbLoop.push(instr)
        HedgeLoop.!!!()
      }
    }

    exchange.subscribeOrders { ord => cache.update(ord) }

    exchange.subscriberBalances(HedgeLoop.push)
  }

}

object Arb {

  object SideFormats
      extends CustomSerializer[Side]({ _ =>
        (
          { case JString(s) => Side(s) },
          { case x: Side => JString(x.toString) }
        )
      })

  case class InstConf(
      curr:           Currency,
      maxOrdQty:      Quantity,
      profitRatio:    Ratio,
      minProfitRatio: Ratio,
      midPxSpread:    Ratio
  )

  def main(args: Array[String]): Unit = {

    Thread.setDefaultUncaughtExceptionHandler { (t, e) =>
      Log.error(e)(s"Thread-${t.getId}: ${t.getName}")
      halt(1)
    }

    JSON.register(SideFormats)

    JSON.registerEnums(Type, Status, TimeInForce)

    val fname = if (args.isEmpty) "arb" else args(0)
    loadProps(s"config/.$fname.properties")

    val exName     = "ex.name".prop
    val targetCurr = "target.currency".prop
    val bridgeCurr = "bridge.currency".prop

    val exchange   = Exchange(exName)
    val instrConfs = CSV.read[InstConf](new File("config/triangle/conf.csv")).asMap(_.curr)
    val instrs     = exchange.instrs.values
    instrs.foreach { i =>
      if (i.quote == targetCurr) {
        instrConfs.get(i.base).foreach { c =>
          i.maxOrdQty      = c.maxOrdQty
          i.profitRatio    = c.profitRatio
          i.minProfitRatio = c.minProfitRatio
          i.midPxSpread    = c.midPxSpread
        }
      } else if (i.quote == bridgeCurr) {
        i.midPxSpread = "0.001".d
      }
    }

    val cInstr  = instrs.find(i => i.base == targetCurr && i.quote == bridgeCurr).get
    val aInstrs = instrs.filter(i => i.quote == targetCurr && instrConfs.contains(i.base)).asMap(_.base)
    val bInstrs = instrs.filter(i => i.quote == bridgeCurr && instrConfs.contains(i.base)).asMap(_.base)

//    (aInstrs.values ++ bInstrs.values ++ List(cInstr)).foreach { instr =>
//      println(instr)
//    }

    new Arb(targetCurr, bridgeCurr, exName, aInstrs, bInstrs, cInstr).start()
  }
}
