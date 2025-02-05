ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.3.3"

val http4sVersion = "0.23.27"


lazy val root = (project in file("."))
  .settings(
    name := "http4sExample",
    libraryDependencies ++= Seq(

      "org.http4s" %% "http4s-ember-client" % http4sVersion,
      "org.http4s" %% "http4s-ember-server" % http4sVersion,
      "org.http4s" %% "http4s-dsl"          % http4sVersion,
      "org.typelevel" %% "cats-effect" % "3.5.1",

      "org.http4s" %% "http4s-circe" % http4sVersion,
      "io.circe" %% "circe-generic" % "0.14.8",
//      "io.circe" %% "circe-literal" % "0.14.8",

      "org.typelevel" %% "cats-core" % "2.12.0",
      "org.typelevel" %% "cats-effect" % "3.5.4",

      "ch.qos.logback" % "logback-classic" % "1.5.6",

      "org.scalatest"     %% "scalatest"                % "3.2.18"        % Test
    )
  )
