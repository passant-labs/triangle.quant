package demo

import chaospi.triangle.entity.{Buy, Order, Side}
import extension.data.JSON
import extension.math.String2Decimal
import org.json4s.native.{JsonMethods, Serialization}
import org.json4s.CustomSerializer
import org.json4s.JsonAST.JString

object JsonDemo {

  object SideFormats
      extends CustomSerializer[Side]({ _ =>
        (
          { case JString(s) => Side(s) },
          { case x: Side => JString(x.toString) }
        )
      })

  JSON.register(SideFormats)

  JSON.registerEnums(Order.Type, Order.Status, Order.TimeInForce)
  val jsonStr =
    """
      |{
      |"a": null,
      |"b": 3,
      |"typ": "Limit",
      |"side": "Buy"
      |}
      |""".stripMargin

  case class Foo(a: String, b: BigDecimal, typ: Order.Type, side: Side)

  def main(args: Array[String]): Unit = {
    println(JSON.write(Foo("a", "3.0".d, Order.Type.Limit, Buy)))
    println(JSON.read[Foo](jsonStr))
  }
}
