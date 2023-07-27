package extension

import java.util.concurrent.CompletionStage
import java.util.{Timer, TimerTask}
import scala.compat.java8.FutureConverters.CompletionStageOps
import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.duration.Duration
import scala.concurrent.ExecutionContext.Implicits.global

package object concurrent {

  implicit def javaFuture2Scala[T](s: CompletionStage[T]): Future[T] = s.toScala

  Thread.setDefaultUncaughtExceptionHandler { (_, e) =>
    e.printStackTrace()
    System.exit(1)
  }

  def thread(cb: => Unit): Thread = {
    val th = new Thread(() => cb)
    th.start()
    th
  }

  implicit class FutureOps[+T](f: Future[T]) {
    def await(duration: Duration = Duration.Inf): T = Await.result(f, duration)
  }

  lazy val timer = new Timer()

  def schedule(delay: Long)(task: => Unit): Unit = {
    timer.schedule(
      new TimerTask {
        def run(): Unit = task
      },
      delay
    )
  }

  def schedule(delay: Long, period: Long)(task: => Unit): Unit = {
    timer.schedule(
      new TimerTask {
        def run(): Unit = task
      },
      delay,
      period
    )
  }

  type RetryFn[-T] = (T, Throwable) => Boolean

  def noRetry[T]: RetryFn[T] = (_, _) => false

  def retry[T](tryCnt: Int)(impl: RetryFn[T]): RetryFn[T] = {
    var cnt = tryCnt
    def wrapped(x: T, e: Throwable): Boolean = {
      if (cnt > 0 && impl(x, e)) {
        cnt -= 1
        true
      } else
        false
    }
    wrapped
  }

  def retryWith[T](retry: RetryFn[T])(pf: => Future[T]): Future[T] = {
    val ret = Promise[T]()
    def tryIt(): Unit = {
      pf map { x =>
        if (retry(x, null)) tryIt() else ret.success(x)
      } recover { e =>
        if (retry(null.asInstanceOf[T], e)) tryIt() else ret.failure(e)
      }
    }
    tryIt()
    ret.future
  }

  def repeat(n: Int)(f: => Unit): Unit = {
    var cnt = n
    while (cnt > 0) {
      cnt -= 1
      f
    }
  }

}
