import sbt._
import Keys._

import Dependencies._
import BuildSettings._

object BuildSettings {
  val buildOrganization = "com.github.scalapecs"
  val buildVersion      = "0.1-SNAPSHOT"
  val buildScalaVersion = "2.9.1"
  val latest            = "latest.integration"

  val buildSettings = Defaults.defaultSettings ++ Seq (
    organization := buildOrganization,
    version      := buildVersion,
    scalaVersion := buildScalaVersion
  )
}

object PecsBuild extends Build {
  lazy val root = Project ( "scalapecs", file ("."),
    settings = buildSettings ++ Seq (
      libraryDependencies ++= Seq (
        dispatchCore,
        dispatchHttp,
        dispatchJson,
        dispatchHttpJson
      ),
      initialCommands in Compile in console := """import pecs._"""
    )
  )
}

object Dependencies {
  lazy val dispatchVersion = "0.8.7"

  lazy val dispatchCore     = "net.databinder" %% "dispatch-core"      % dispatchVersion
  lazy val dispatchHttp     = "net.databinder" %% "dispatch-http"      % dispatchVersion
  lazy val dispatchJson     = "net.databinder" %% "dispatch-json"      % dispatchVersion
  lazy val dispatchHttpJson = "net.databinder" %% "dispatch-http-json" % dispatchVersion
}
