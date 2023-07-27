package mage.triangle.exchanges

import mage.triangle.entity.ExName

sealed abstract class HttpError(exName: ExName, errMsg: String) extends RuntimeException(s"$exName: $errMsg")

case class OrderNonexistent(exName: ExName, errMsg: String) extends HttpError(exName, errMsg)

case class PostOnlyMatched(exName: ExName, errMsg: String) extends HttpError(exName, errMsg)

case class AccessRateLimited(exName: ExName, errMsg: String) extends HttpError(exName, errMsg)

case class InsufficientAsset(exName: ExName, errMsg: String) extends HttpError(exName, errMsg)

case class MinLotLimited(exName: ExName, errMsg: String) extends HttpError(exName, errMsg)

case class BadlyFormed(exName: ExName, errMsg: String) extends HttpError(exName, errMsg)

case class TemporaryPause(exName: ExName, errMsg: String) extends HttpError(exName, errMsg)

case class AuthenticationFailure(exName: ExName, errMsg: String) extends HttpError(exName, errMsg)

case class Unknown(exName: ExName, errMsg: String) extends HttpError(exName, errMsg)

trait OrderRejected extends HttpError
