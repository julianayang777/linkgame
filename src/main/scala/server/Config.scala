package server

import com.comcast.ip4s.{Host, Hostname, Port}
import pureconfig._
import pureconfig.module.ip4s._
import pureconfig.error.CannotConvert
import pureconfig.generic.semiauto.deriveReader
import server.Config.SecretConfigValue

import scala.concurrent.duration.FiniteDuration

case class Config(
  serverPort: Port,
  serverHost: Host,
  jwtSecret: SecretConfigValue[String],
  jwtExpirationTime: FiniteDuration,
)

object Config {
  case class SecretConfigValue[T](value: T) extends AnyVal {
    override def toString: String = "secret"
  }

  object SecretConfigValue {
    implicit def secretConfigValueReader[T: ConfigReader]: ConfigReader[SecretConfigValue[T]] =
      ConfigReader[T].map(SecretConfigValue.apply)
  }

  implicit val configReader: ConfigReader[Config] = deriveReader

  implicit val hostnameReader: ConfigReader[Host] =
    ConfigReader.fromString(str =>
      Hostname.fromString(str) match {
        case Some(hostname) => Right(hostname)
        case None           => Left(CannotConvert(str, "Hostname", "Invalid hostname"))
      }
    )

}
