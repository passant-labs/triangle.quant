package mage.triangle.entity

trait Memoize[K, V] {

  @volatile final var VALUES: Map[K, V] = Map[K, V]()

  def apply(k: K): V = {
    val v = VALUES.get(k)
    if (v.isDefined) return v.get
    synchronized {
      val v = VALUES.get(k)
      if (v.isDefined) return v.get
      val newv = make(k)
      VALUES += k -> newv
      newv
    }
  }

  def make(k: K): V

}
