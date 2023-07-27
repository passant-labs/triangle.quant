package mage.triangle.exchanges

import extension.io.WebSocketDecoder
import extension.logging.Log
import extension.system.halt

import java.net.URI
import java.net.http.HttpClient

abstract class WebSocket[T](
    uri:           URI,
    subprotocols:  List[String]           = List(),
    headers:       List[(String, String)] = List(),
    autoReconnect: Boolean = true
)(implicit decode: WebSocketDecoder[T], client: HttpClient = HttpClient.newBuilder().build())
    extends extension.io.WebSocket[T](uri, subprotocols, headers, autoReconnect) {
  override def onError(e: Throwable): Unit = {
    try {
      super.onError(e)
    } catch {
      case e: Throwable =>
        halt(1)
        Log.error(e)(s"Unknown WebSocket $uri")
    }
  }
}
