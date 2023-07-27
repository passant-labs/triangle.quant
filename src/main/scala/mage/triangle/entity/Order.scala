package mage.triangle.entity

import extension.math.ZERO
import extension.system.nowMs

object Order {
  type Type        = Type.Value
  type Status      = Status.Value
  type TimeInForce = TimeInForce.Value
  type ExecInst    = Long

  object Type extends Enumeration {
    val Limit, StopLimit, LimitIfTouched = Value
    val Market, Stop, MarketIfTouched    = Value
  }

  object TimeInForce extends Enumeration {
    val GTC, IOC, FOK = Value
  }

  object Status extends Enumeration {
    val PendingNew, New, Closed = Value
  }

  object ExecInst {
    final val NoSet                    = 0L
    final val PostOnly                 = 1L
    final val ParticipateDoNotInitiate = PostOnly
  }

}

case class Order(
    exName:       String,
    code:         Code,
    side:         Side,
    typ:          Order.Type        = Order.Type.Limit,
    px:           Price,
    qty:          Quantity,
    clID:         ID                = newClID(),
    var fxID:     OrderID           = null,
    var txTs:     Long              = nowMs(),
    tif:          Order.TimeInForce = Order.TimeInForce.GTC,
    execInst:     Order.ExecInst    = Order.ExecInst.NoSet,
    var cumQty:   Quantity          = ZERO,
    var cumValue: Amount            = ZERO,
    var status:   Order.Status      = Order.Status.PendingNew,
    var text:     String            = null
) {

  val id:        Long       = clID
  val instr:     Instrument = Instrument(code, exName)
  val sym:       Sym        = instr.sym
  def leavesQty: Quantity   = qty - cumQty

  def opening: Boolean = status < Order.Status.Closed
  def closed:  Boolean = !opening
  def close(yes: Boolean): Boolean = {
    if (yes) status = Order.Status.Closed
    closed
  }

  def isLimit:  Boolean = typ < Order.Type.Market
  def isMarket: Boolean = !isLimit

  def hasSet(ins: Order.ExecInst): Boolean = (execInst & ins) == ins

  def postOnly: Boolean = hasSet(Order.ExecInst.PostOnly)

  override def toString: String = {
    s"$exName $code $side px=$px qty=$qty cumQty=$cumQty status=$status id=$id fxID=$fxID"
  }

  def statusNewerThan(existing: Order): Boolean = {
    existing == null ||
    (this.status > existing.status) ||
    (this.cumQty > existing.cumQty)
  }

  def fill(patch: Order): Unit = {
    this.fxID = patch.fxID

    this.status   = patch.status
    this.cumQty   = patch.cumQty
    this.cumValue = patch.cumValue

    if (patch.txTs > 0)
      this.txTs = patch.txTs

    this.text =
      if (this.text == null) patch.text
      else if (patch.text != null && !this.text.contains(patch.text))
        this.text + s"; ${patch.text}"
      else
        this.text
  }

  override def canEqual(that: Any): Boolean = {
    that.isInstanceOf[this.type]
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || !canEqual(obj)) return false
    val that = obj.asInstanceOf[this.type]
    this.id == that.id
  }

  override def hashCode(): Int = this.id.hashCode()

}
