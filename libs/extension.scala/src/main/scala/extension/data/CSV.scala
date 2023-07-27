package extension.data

import com.fasterxml.jackson.databind.{ObjectMapper, ObjectReader}
import com.fasterxml.jackson.dataformat.csv.{CsvMapper, CsvParser, CsvSchema}

import java.io.{File, InputStream}
import java.util

object CSV {

  type StringKV = util.Map[String, String]

  val tmpClass: Class[StringKV] = classOf[util.Map[String, String]]

  val mapper = new CsvMapper()
  val json   = new ObjectMapper()

  val schema: CsvSchema = mapper
    .configure(CsvParser.Feature.TRIM_SPACES, true)
    .schemaFor(tmpClass)
    .withHeader
    .withComments()

  val reader: ObjectReader = mapper
    .readerFor(classOf[java.util.Map[String, AnyVal]])
    .`with`(schema)

  def read[T](in: InputStream)(implicit mf: Manifest[T]): List[T] = {
    val maps = reader.readValues[StringKV](in).readAll()
    JSON.read[List[T]](JSON.write(maps))
  }

  def read[T](f: File)(implicit mf: Manifest[T]): List[T] = {
    val maps = reader.readValues[StringKV](f).readAll()
    val str  = json.writeValueAsString(maps)
    JSON.read[List[T]](str)
  }
}
