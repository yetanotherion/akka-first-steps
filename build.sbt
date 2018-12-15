name := "akka-trial"

version := "1.0"

scalaVersion := "2.12.6"

lazy val akkaVersion = "2.5.18"
lazy val akkaHttpVersion = "10.1.5"

scalafmtOnCompile := true

scalacOptions += "-Xfatal-warnings"
scalacOptions += "-Ywarn-unused-import"
scalacOptions += "-unchecked"

libraryDependencies ++= Seq(
  "com.typesafe.akka" %% "akka-actor" % akkaVersion,
  "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
  "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-stream" % akkaVersion,
  "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
  "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion,
  "org.scalatest" %% "scalatest" % "3.0.5" % "test"
)
