ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.16"

val catsVersion        = "2.10.0"
val catsEffect3Version = "3.3.0"
val http4sVersion      = "0.23.18"
val circeVersion       = "0.14.6"
val JwtHttp4sVersion   = "1.2.0"
val JwtScalaVersion    = "9.3.0"
val pureConfigVersion  = "0.17.2"

libraryDependencies ++= Seq(
  "org.typelevel"         %% "cats-core"              % catsVersion,
  "org.typelevel"         %% "cats-effect"            % catsEffect3Version,
  "io.circe"              %% "circe-core"             % circeVersion,
  "io.circe"              %% "circe-generic"          % circeVersion,
  "io.circe"              %% "circe-generic-extras"   % "0.14.4",
  "io.circe"              %% "circe-parser"           % circeVersion,
  "org.http4s"            %% "http4s-dsl"             % http4sVersion,
  "org.http4s"            %% "http4s-ember-server"    % http4sVersion,
  "org.http4s"            %% "http4s-ember-client"    % http4sVersion,
  "org.http4s"            %% "http4s-circe"           % http4sVersion,
  "ch.qos.logback"         % "logback-classic"        % "1.5.16",
  "dev.profunktor"        %% "http4s-jwt-auth"        % JwtHttp4sVersion,
  "com.github.jwt-scala"  %% "jwt-circe"              % JwtScalaVersion,
  "com.github.pureconfig" %% "pureconfig"             % pureConfigVersion,
  "com.github.pureconfig" %% "pureconfig-cats-effect" % pureConfigVersion,
  "com.github.pureconfig" %% "pureconfig-ip4s"        % pureConfigVersion
)

lazy val root = (project in file("."))
  .settings(
    name := "linkgame"
  )
