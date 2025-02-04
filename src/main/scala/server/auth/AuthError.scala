package server.auth

import scala.util.control.NoStackTrace

sealed trait AuthError extends Throwable

case object UserAlreadyLoggedIn extends AuthError with NoStackTrace
case object UserAlreadyExists   extends AuthError with NoStackTrace
case object UserNotLoggedIn     extends AuthError with NoStackTrace
case object UserNotFound        extends AuthError with NoStackTrace
case object UnexpectedError     extends AuthError with NoStackTrace
