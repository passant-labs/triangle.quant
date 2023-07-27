package extension.data

import org.json4s.{
  CustomSerializer,
  DefaultFormats,
  DefaultReaders,
  Formats,
  JDecimal,
  JString,
  Reader,
  Serializer
}
import org.json4s.ext.EnumNameSerializer
import org.json4s.native.{JsonMethods, Serialization}

import java.io.InputStream

object JSON extends DefaultReaders {

  type JValue = org.json4s.JValue

  implicit object DecimalReader extends Reader[BigDecimal] {
    def read(value: JValue): BigDecimal =
      value match {
        case JString(s) => BigDecimal(s)
        case x          => BigDecimalReader.read(x)
      }
  }

  object BigDecimalSerializer
      extends CustomSerializer[BigDecimal]({ _ =>
        (
          { case x: JValue => DecimalReader.read(x) },
          { case x: BigDecimal => JDecimal(x) }
        )
      })

  implicit var formats: Formats = DefaultFormats + BigDecimalSerializer

  def registerEnums(es: Enumeration*): Unit = {
    es.foreach(e => formats += new EnumNameSerializer(e))
  }

  def register(fmt: Serializer[_]): Unit = {
    formats += fmt
  }

  def parse(s: String): JValue = JsonMethods.parse(s, useBigDecimalForDouble = true)

  def parse(in: InputStream): JValue = JsonMethods.parse(in, useBigDecimalForDouble = true)

  def read[T](s: String)(implicit mf: scala.reflect.Manifest[T]): T = {
    Serialization.read[T](s)
  }

  def write[T](x: T): String = {
    Serialization.write[T](x)
  }
}
