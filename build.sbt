name := "FinalProject"

version := "1.0"

scalaVersion := "2.11.7"

resolvers += "spray repo" at "http://repo.spray.io"

libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-actor" % "2.3.13",
  "io.spray" %% "spray-can" % "1.3.3",
  "io.spray" %% "spray-routing" % "1.3.3",
  "io.spray" %% "spray-testkit" % "1.3.3" % "test",
  "org.scalatest" % "scalatest_2.11" % "2.2.4" % "test",
  "com.typesafe.akka" %% "akka-testkit" % "2.3.13")
    