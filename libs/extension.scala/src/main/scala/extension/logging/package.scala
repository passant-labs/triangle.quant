package extension

import extension.system.StringConversation
import org.log4s.{getLogger, Logger}

package object logging {
  final val Log: Logger = getLogger("app".prop("ILogger"))
}
