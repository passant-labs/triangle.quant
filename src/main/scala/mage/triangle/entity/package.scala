package mage.triangle

import extension.system.nowMs

package object entity {
  type ID       = Long
  type OrderID  = String
  type Price    = BigDecimal
  type Amount   = BigDecimal
  type Quantity = BigDecimal
  type Ratio    = BigDecimal

  type Code      = String
  type CodeOrSym = String
  type Sym       = String
  type Currency  = String
  type ExName    = String

  @volatile var LARGEST_CLID: Long = nowMs() * 1000
  private final val ID_GEN_LOCK = new Object
  def newClID(): ID = {
    ID_GEN_LOCK.synchronized {
      val newID = (nowMs() * 1000).max(LARGEST_CLID + 1)
      LARGEST_CLID = newID
      newID
    }
  }
}
