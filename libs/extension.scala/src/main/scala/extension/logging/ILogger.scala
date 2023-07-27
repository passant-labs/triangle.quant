package extension.logging

import org.log4s.{getLogger, Logger}

trait ILogger {
  val log: Logger = getLogger(s"${this.getClass.getSimpleName}")
}
