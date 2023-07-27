package mage

import mage.triangle.entity.{ID, Order}

import scala.util.matching.Regex

package object triangle {
  val SrcOrdRegex: Regex = "SrcOrdID=(\\w+)".r
  def srcOrdID(ord: Order): ID = {
    if (ord.text == null) return 0L
    SrcOrdRegex
      .findFirstMatchIn(ord.text)
      .map(_.group(1).toLong)
      .getOrElse(0L)
  }
}
