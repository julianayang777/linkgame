package server.player

import scala.util.control.NoStackTrace

sealed trait PlayerError extends Throwable

case object PlayerAlreadyExists extends PlayerError with NoStackTrace
case object PlayerNotFound      extends PlayerError with NoStackTrace
