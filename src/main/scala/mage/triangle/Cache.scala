package mage.triangle

import mage.triangle.entity.{Code, Currency, ID, OrderID, Quantity}
import com.redis.RedisClient
import extension.data.JSON
import extension.math.ZERO
import mage.triangle.entity.{Instrument, Order}

import java.net.URI
import scala.collection.mutable

class Cache(cInstr: Instrument) {

  protected[this] val orders: mutable.Map[ID, Order] = mutable.Map[ID, Order]()

  def initialize(os: Iterable[Order]): Unit = {
    os.foreach(o => orders += o.id -> o)
  }

  def insert(ord: Order): Unit = synchronized {
    orders += ord.id -> ord
  }

  def update(o: Order): Boolean = synchronized {
    val ord = orders.getOrElse(o.id, return false)

    if (!o.statusNewerThan(ord)) return false

    ord.fill(o)
    if (!ord.close(ord.leavesQty == ZERO)) return true

    orders -= ord.id
    true
  }

  def delete(ord: Order): Unit = synchronized {
    orders -= ord.id
  }

  def getOrders: Iterable[Order] = synchronized {
    orders.values
  }

  def load(): Unit = {}
}

trait Storage {

  def dump(ord: Order)

  def del(ord: Order)

  def orders: Iterable[Order]

}

class PersistentCache(cInstr: Instrument, val storage: Storage) extends Cache(cInstr) {
  override def insert(ord: Order): Unit = synchronized {
    super.insert(ord)
    storage.dump(ord)
  }

  override def update(ord: Order): Boolean = synchronized {
    if (!super.update(ord)) return false
    if (ord.closed) storage.del(ord) else storage.dump(ord)
    true
  }

  override def delete(ord: Order): Unit = synchronized {
    super.delete(ord)
    storage.del(ord)
  }

  override def load(): Unit = {
    initialize(storage.orders)
  }
}

class RedisStorage private (redis: RedisClient) extends Storage {

  def this(uri: URI) = this(new RedisClient(uri))

  private val OrdersKey = "orders"

  def dump(ord: Order): Unit = {
    redis.hset(OrdersKey, ord.id, JSON.write(ord))
  }

  def del(ord: Order): Unit = {
    redis.hdel(OrdersKey, ord.id)
  }

  def orders: Iterable[Order] = {
    redis.hgetall[String, String](OrdersKey).getOrElse(Map.empty).map { kv =>
      JSON.read[Order](kv._2)
    }
  }
}
