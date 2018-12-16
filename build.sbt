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
  "javax.ws.rs" % "javax.ws.rs-api" % "2.0.1",
  "com.github.swagger-akka-http" %% "swagger-akka-http" % "2.0.0",
  "com.github.swagger-akka-http" %% "swagger-scala-module" % "2.0.2",
  "ch.megard" %% "akka-http-cors" % "0.3.0",
  "org.scalatest" %% "scalatest" % "3.0.5" % "test",
  "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"
)
