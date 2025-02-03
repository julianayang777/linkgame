package server.auth

import scala.util.control.NoStackTrace

sealed trait AuthError extends Throwable

case object UserAlreadyExists extends AuthError with NoStackTrace
