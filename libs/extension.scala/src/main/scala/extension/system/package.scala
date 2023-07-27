package extension

import java.io.File
import java.net.URL
import java.util.Properties
import scala.util.Random

package object system {

  implicit final class StringConversation(s: String) {

    def resource: URL = Thread.currentThread().getContextClassLoader.getResource(s)

    def prop: String = {
      val p = System.getProperty(s)
      if (p == null) {
        val k = s.replace('.', '_').toUpperCase
        System.getenv(k)
      } else p
    }

    def prop(default: => String): String = {
      val v = this.prop
      if (v != null) v else default
    }

  }

  def randString(n: Int): String = {
    Random.alphanumeric.take(n).mkString("")
  }

  def halt(status: Int): Unit = Runtime.getRuntime.halt(status)

  @inline def nowMs():      Long = System.currentTimeMillis()
  @inline def nowSeconds(): Long = System.currentTimeMillis() / 1000L

  def loadProps(path: String): Unit = {
    loadProps(new File(path).toURI.toURL)
  }

  def loadProps(url: URL): Unit = {
    val tmp = new Properties()
    tmp.load(url.openStream())
    tmp.putAll(System.getProperties)
    System.setProperties(tmp)
  }

}
