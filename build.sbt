name := "mage.triangle"

version := "0.1"

scalaVersion := "2.13.4"

lazy val root      = (project in file(".")).dependsOn(extension)
lazy val extension = project in file("libs/extension.scala")

libraryDependencies += "ch.qos.logback" % "logback-classic" % "1.2.3"

libraryDependencies += "net.debasishg" %% "redisclient" % "3.30"
