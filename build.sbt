name := "codeguru-extreme"

version := "5.0"

scalaVersion := "2.12.1"

coverageEnabled := true

libraryDependencies += "org.scalactic" %% "scalactic" % "3.0.1"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.1" % "test"

import de.johoop.findbugs4sbt.FindBugs._

findbugsSettings