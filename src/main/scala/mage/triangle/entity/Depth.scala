package mage.triangle.entity

import extension.collection.SortedSetOps
import extension.math.{BigDecimalOps, Inf, TWO, ZERO}
import extension.system.nowMs

import java.util
import java.util.concurrent.ConcurrentSkipListSet

class Depth(
    val instr: Instrument
) {

  @volatile var updatedTs: ID = nowMs()

  val BidInf: Quote = Quote(instr, Buy, -Inf, ZERO)
  val AskInf: Quote = Quote(instr, Sell, Inf, ZERO)

  private final val BUF = new util.LinkedList[Quote]()

  final val bids = new ConcurrentSkipListSet[Quote]()

  final val asks = new ConcurrentSkipListSet[Quote]()

  private def isInf(q: Quote): Boolean = q.eq(AskInf) || q.eq(BidInf)

  private def insert(q: Quote): Any = {
    val qs = if (q.side.isBuy) bids else asks
    if (isInf(q)) return qs.clear()

    qs.remove(q)
    qs.add(q)
  }

  private def delete(q: Quote): Boolean = {
    val qs = if (q.side.isBuy) bids else asks
    qs.remove(q)
  }

  def clear(side: Side = Side.Both): Unit = {
    if (side != Buy) BUF.add(AskInf)
    if (side != Sell) BUF.add(BidInf)
  }

  def put(q: Quote): Unit = {
    BUF.add(q)
  }

  def commit(): Unit =
    synchronized {
      val qs = BUF.iterator()
      while (qs.hasNext) {
        val q = qs.next()
        insert(q)
        qs.remove()
      }
    }

  private def bestOn(qs: ConcurrentSkipListSet[Quote]): Quote =
    synchronized {
      qs.firstOr(if (qs.eq(bids)) BidInf else AskInf)
    }

  def bid: Quote = bestOn(bids)

  def ask: Quote = bestOn(asks)

  def bestOn(side: Side): Quote = if (side.isBuy) bestOn(bids) else bestOn(asks)

  def bestFor(side: Side): Quote = bestOn(-side)

  def best: (Quote, Quote) = (bid, ask)

  def bestPxs: (Price, Price) = (bid.px, ask.px)

  def midpx: Price = {
    val (bpx, apx) = bestPxs
    if (bpx.abs.eq(Inf) && apx.abs.eq(Inf)) return instr.tick
    if (bpx.abs.eq(Inf)) return apx
    if (apx.abs.eq(Inf)) return bpx
    (bpx + apx) / TWO
  }

  def midpx(side: Side): Price = midpx.roundWith(instr.tick, side.dirRound)

  def bidPx: Price = bid.px

  def askPx: Price = ask.px

  def tapPx(side: Side): Price =
    bestFor(side).px - side.dirSign * instr.tick
}

object Depth extends Memoize[Instrument, Depth] {
  def apply(): Depth = apply(Instrument.empty)
  def make(instr: Instrument): Depth = new Depth(instr)
}
