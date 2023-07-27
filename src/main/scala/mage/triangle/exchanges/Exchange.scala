package mage.triangle.exchanges

import mage.triangle.entity.{Balance, Code, Depth, ID, Instrument, Memoize, Order, OrderID, Sym}
import scala.concurrent.Future

trait Exchange {

  final val exName: String = this.getClass.getSimpleName

  def instrs: Map[Code, Instrument]

  def placeOrder(ord: Order): Future[OrderID]

  def cancelOrder(ord: Order): Unit = cancelOrder(fxID = ord.fxID, clID = ord.clID)

  protected def checkIDS(fxID: OrderID, clID: ID): Unit = {
    if (fxID == null && clID <= 0) throw new IllegalArgumentException("fxID | clID must have one value")
  }

  def cancelOrder(fxID: OrderID = null, clID: ID = 0L)

  def fetchOrder(fxID: OrderID = null, clID: ID = 0L): Future[Order]

  def fetchOpeningOrders(): Future[List[Order]]

  def fetchBalances(): Future[List[Balance]]

  def subscribeDepth(code: Code = null, sym: Sym = null)(cb: Depth => Unit): Unit = {
    val instr =
      (instrs.get(code) orElse instrs.get(sym))
        .getOrElse(throw new IllegalStateException(s"No instrument for $code | $sym"))
    val d = Depth(instr)
    subscribeDepth(instr.sym, d)(cb)
  }

  def subscribeDepth(sym: Sym, depth: Depth)(cb: Depth => Unit): Unit

  def subscribeOrders(cb: Order => Unit)

  def subscriberBalances(cb: Balance => Unit)

}

object Exchange extends Memoize[String, Exchange] {

  private final val pkg = this.getClass.getPackageName

  def make(exName: String): Exchange = {
    Class
      .forName(s"$pkg.$exName")
      .getDeclaredConstructor()
      .newInstance()
      .asInstanceOf[Exchange]
  }
}
