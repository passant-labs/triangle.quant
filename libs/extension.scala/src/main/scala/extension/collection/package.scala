package extension

import java.util

package object collection {
  implicit final class MapOps[K, V](self: Map[K, V]) {
    def mergeBy(that: Map[K, V])(op: (V, V) => V): Map[K, V] = {
      var sum = self
      for ((k, v) <- that) {
        val n = sum.get(k).map(op(_, v)).getOrElse(v)
        sum += k -> n
      }
      sum
    }
  }

  implicit final class SortedSetOps[E](s: util.SortedSet[E]) {
    def firstOr(default: => E): E =
      try {
        s.first()
      } catch {
        case _: NoSuchElementException => default
      }

    def firstOrNull: E = firstOr(null.asInstanceOf[E])
  }

  implicit final class ToMap[E](xs: Iterable[E]) {

    def asMap[K](kf: E => K): Map[K, E] = {
      xs.map(v => (kf(v), v)).toMap
    }

    def xMap[K, V](kvf: E => (K, V)): Map[K, V] = {
      xs.map(kvf).toMap
    }

  }
}
