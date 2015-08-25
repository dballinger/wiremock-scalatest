name := "wiremock-scalatest"

version := "1.0"

scalaVersion := "2.11.7"

libraryDependencies ++= Seq(
  "com.github.tomakehurst" % "wiremock" % "1.57" % "test,provided"
)