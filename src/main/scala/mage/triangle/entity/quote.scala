package mage.triangle.entity

import extension.math.{BigDecimalOps, ONE, ZERO}

import scala.math.BigDecimal.RoundingMode.{DOWN, RoundingMode, UP}

sealed trait Side {
  def unary_- : Side
  def unary_! : Side   = -this
  def isBuy:   Boolean = this == Buy
  def isSell:  Boolean = this == Sell
  def apply(px: Price): Q = Q0(this, px)
  def dirSign:  Int
  def dirRound: RoundingMode
}

case object Buy extends Side {
  override def unary_-  : Side        = Sell
  override val dirSign:  Int          = 1
  override val dirRound: RoundingMode = DOWN
}

case object Sell extends Side {
  override def unary_-  : Side        = Buy
  override val dirSign:  Int          = -1
  override val dirRound: RoundingMode = UP
}

object Side {

  val Both: Side = null

  def apply(s: String): Side =
    s match {
      case "Buy" | "BUY" | "buy" | "bid" | "bids"    => Buy
      case "Sell" | "SELL" | "sell" | "ask" | "asks" => Sell
    }
}

trait Q extends Ordered[Q] {
  val side: Side
  val px:   Price

  def comparePx: Price = px

  override def compare(q: Q): Int = {
    assert(this.side == q.side)
    side.dirSign * (q.comparePx compare this.comparePx)
  }
}

case class Q0(side: Side, px: Price) extends Q

case class Quote(side: Side, px: Price, qty: Quantity, takerCostPx: Price) extends Q

object Quote {
  def apply(instr: Instrument, side: Side, px: Price, qty: Quantity): Quote = {
    Quote(side, px, qty, instr.takerCostPx(-side, px))
  }
}

case class Instrument(
    code:               Code,
    exName:             String,
    sym:                Sym,
    base:               Currency,
    quote:              Currency,
    tick:               Price,
    lot:                Quantity,
    var takerCommRate:  Ratio,
    var makerCommRate:  Ratio,
    var minOrdValue:    Amount   = ZERO,
    var maxOrdQty:      Quantity = ZERO,
    var maxPosQty:      Quantity = ZERO,
    var profitRatio:    Ratio    = ZERO,
    var minProfitRatio: Ratio    = ZERO,
    var midPxSpread:    Ratio    = ZERO
) {
  val fluctuation: Amount = tick * lot

  Instrument.intern(this)

  def takerCostPx(side: Side, px: Price): Price = {
    px * (ONE + side.dirSign * takerCommRate)
  }

  def makerPx(side: Side, costPx: Price): Price = {
    (costPx / (ONE + side.dirSign * makerCommRate))
      .roundWith(tick, side.dirRound)

  }

  def quantity(px: Price, value: Amount): Quantity = {
    value.quot(fluctuation).quot(px) * lot
  }

  override def canEqual(that: Any): Boolean = {
    that.isInstanceOf[this.type]
  }

  override def equals(obj: Any): Boolean = {
    if (obj == null || !canEqual(obj)) return false
    val that = obj.asInstanceOf[this.type]
    this.exName == that.exName && this.code == that.code
  }

  override def hashCode(): Int =
    exName.hashCode | code.hashCode

}

object Instrument {

  @volatile var INSTRS: Map[(Code, String), Instrument] = Map[(Code, String), Instrument]()

  def apply(code: Code, exName: String): Instrument = INSTRS((code, exName))

  def intern(instr: Instrument): Unit =
    synchronized {
      INSTRS += (instr.code, instr.exName) -> instr
    }

  object empty
      extends Instrument(
        code          = null,
        exName        = null,
        sym           = null,
        base          = null,
        quote         = null,
        tick          = ZERO,
        lot           = ZERO,
        minOrdValue   = ZERO,
        takerCommRate = ZERO,
        makerCommRate = ZERO
      )

}
