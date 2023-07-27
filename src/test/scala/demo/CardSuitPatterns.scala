package demo

import scala.util.Random

object CardSuitPatterns {

  def main(args: Array[String]): Unit = {
    var rs: Iterable[Int] = 1 to 52
    var cnt = 0
    for (_ <- 1 to 1000000) {
      rs = Random.shuffle(rs)
      if (isFourKind(rs)) {
        cnt += 1
      }
    }
    println(cnt)
  }

  def isFourKind(rs: Iterable[Int]): Boolean = {
    rs.take(5).groupBy(_ % 13).values.map(_.size).toSet.contains(4)
  }

}
