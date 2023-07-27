package extension.io

import extension.concurrent.thread
import extension.data.JSON
import extension.data.JSON._
import extension.system.StringConversation
import extension.logging.Log

import java.net.URLEncoder
import scala.collection.mutable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

trait Notifier[T] {
  def notify(msg: T): Future[Boolean]
}

trait Bot[T] {
  val commandsHandlers = mutable.Map[T, T => Unit]()

  def listenCommand(command: T)(handler: T => Unit): Unit

  def subscribeCommands(): Unit
}

class Telegram extends Notifier[String] with Bot[String] {
  private final val chatId         = "chat.id".prop
  private final val token          = "bot.token".prop
  private final val baseURL        = s"https://api.telegram.org/bot$token"
  private final val defaultHeaders = List("Content-Type" -> "application/x-www-form-urlencoded")

  override def notify(msg: String): Future[Boolean] = {
    val escaped = URLEncoder.encode(msg, "UTF-8")
    val data    = s"chat_id=$chatId&text=$escaped&parse_mode=Markdown"
    HTTP
      .request(method = "POST", uri = s"$baseURL/sendMessage", headers = defaultHeaders, body = data)
      .map { rsp =>
        rsp.body().string()
        true
      }
      .recover { e =>
        Log.error(e)("Uncaught Error")
        false
      }
  }

  override def listenCommand(command: String)(handler: String => Unit): Unit = {
    commandsHandlers += command -> handler
  }

  override def subscribeCommands(): Unit = {
    thread {
      var offset: Int = Int.MinValue
      while (true) {
        val data = if (offset > 0) s"offset=$offset" else ""
        val f = HTTP
          .request(method = "POST", uri = s"$baseURL/getUpdates", headers = defaultHeaders, data)
          .map { rsp =>
            val json      = JSON.parse(rsp.body().string())
            val msgs      = (json \ "result").children
            val updateIDS = msgs.map(v => (v \ "update_id").as[Int])
            val updateID  = updateIDS.maxOption.getOrElse(Int.MinValue)
            offset = updateID + 1
            msgs
          }
          .andThen {
            case Success(rst) =>
              rst.foreach { msg =>
                val text    = (msg \ "message" \ "text").as[String]
                val command = commandsHandlers.keys.find(_.startsWith(text)).orNull
                if (command != null)
                  commandsHandlers(command).apply(command)
              }
            case Failure(_) =>
          }

        Await.result(f, Duration.Inf)
      }
    }
  }
}
