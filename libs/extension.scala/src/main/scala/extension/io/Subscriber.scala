package extension.io

import scala.collection.immutable

trait Subscriber[V] {
  @volatile var handlers: Map[String, V => Unit] = immutable.Map[String, V => Unit]()

  def topicOf(v: V): String

  def subscribe(topic: String)(cb: V => Unit): Unit = handlers += topic -> cb

  def unsubscribe(topic: String): Unit = handlers -= topic

  @inline def consume(t: String, v: V): Unit = handlers(t)(v)

  @inline def consume(v: V): Unit = consume(topicOf(v), v)
}
