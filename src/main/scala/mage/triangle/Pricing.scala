package mage.triangle

import mage.triangle.entity.{Price, Ratio}
import extension.math.{clamp, BigDecimalOps, ONE, String2Decimal}
import mage.triangle.entity.{Buy, Depth, Instrument, Order, Sell, Side}

class Pricing(
    val aInstr: Instrument,
    val bInstr: Instrument,
    val cInstr: Instrument
) {

  val ProfitRatio:    Ratio = aInstr.profitRatio
  val MinProfitRatio: Ratio = aInstr.minProfitRatio
  val MidPxSpread:    Ratio = aInstr.midPxSpread
  val commRate:       Ratio = aInstr.makerCommRate //+ bInstr.takerCommRate + cInstr.takerCommRate

  val ad: Depth = Depth(aInstr)
  val bd: Depth = Depth(bInstr)
  val cd: Depth = Depth(cInstr)

  val aSide: Side = side(aInstr, bInstr)
  val bSide: Side = side(bInstr, cInstr)
  val cSide: Side = side(cInstr, aInstr)

  private def side(x: Instrument, y: Instrument): Side = {
    if (x.quote == y.quote || x.quote == y.base) Sell else Buy
  }

  def profitPx(ratio: Ratio): Price = {
    val bpx = bd.bestFor(bSide).px
    val cpx = cd.bestFor(cSide).px
    (
      (ONE + ratio + commRate)
        * bpx.pow(bSide.dirSign)
        * cpx.pow(cSide.dirSign)
    ).pow(-aSide.dirSign)
  }

  def spreadPx(ratio: Ratio): Price =
    ad.midpx * (ONE - aSide.dirSign * ratio)

  def apx(): Price = {
    val px    = profitPx(ProfitRatio)
    val tapPx = ad.tapPx(aSide)
    val sPx   = spreadPx(MidPxSpread)
    clamp(px, sPx, tapPx).roundWith(ad.instr.tick, aSide.dirRound)
  }

  def brokenSpread(px: Price): Boolean = {
    aSide(px) > aSide(spreadPx(MidPxSpread))
  }

  def brokenProfit(px: Price): Boolean = {
    aSide(px) < aSide(profitPx(MinProfitRatio))
  }

  def broken(px: Price): Boolean = {
    brokenSpread(px) || brokenProfit(px)
  }

  def spreadPx(d: Depth, side: Side, ratio: Ratio): Price = d.midpx * (ONE - side.dirSign * ratio)

  def spreadPx(d: Depth, side: Side): Price = spreadPx(d, side, d.instr.midPxSpread)

  def brokenSpread(ord: Order): Boolean = {
    val instr = ord.instr
    val side  = ord.side
    if (instr == aInstr) throw new IllegalArgumentException(s"${ord.sym}")
    if (instr == bInstr) return side(ord.px) > side(spreadPx(bd, side))
    side(ord.px) > side(spreadPx(cd, side))
  }

  def bpx(ao: Order): Price = {
    bd.bestFor(bSide).px
  }

  def cpx(ao: Order, bo: Order): Price = {
    cd.bestFor(cSide).px
  }

  def makerPx(d: Depth, side: Side): Price = Array(side(d.tapPx(side)), side(d.midpx(side))).min.px

  def bMakerPx(): Price = makerPx(bd, bSide)

  def cMakerPx(): Price = makerPx(cd, cSide)

}
