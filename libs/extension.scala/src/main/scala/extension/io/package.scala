package extension

import java.io.{ByteArrayInputStream, Closeable, InputStream}
import java.lang.reflect.Field
import java.net.{URI, URL}
import java.nio.ByteBuffer
import java.util.zip.GZIPInputStream
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration

package object io {

  implicit final class AutoClose[T <: Closeable, R](f: T) {
    def apply(body: T => R): R =
      try body(f)
      finally f.close()
  }

  val f: Field = classOf[ByteBuffer].getDeclaredField("hb")
  f.setAccessible(true)
  implicit class ByteBufferOps(bb: ByteBuffer) {
    def bytes: Array[Byte] =
      if (bb.isReadOnly) f.get(bb).asInstanceOf[Array[Byte]] else bb.array()
    def inputStream = new ByteArrayInputStream(bytes)
  }

  implicit class InputStreamOps(in: InputStream) {
    def gzip = new GZIPInputStream(in)
  }

  implicit def String2URI(s:   String): URI    = URI.create(s)
  implicit def URI2String(uri: URI):    String = uri.toString
  implicit def URL2String(url: URL):    String = url.toString

}
