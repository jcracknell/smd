import sbt._
import Keys._

object Dependencies {
  def scalatest = "org.scalatest" %% "scalatest" % "1.9+"
}

object SmdBuild extends Build {
  import Dependencies._

  val baseSettings = Defaults.defaultSettings ++ Seq(
    version :=      "0.1",
    scalaVersion := "2.10.2",
    libraryDependencies <++= (scalaVersion) { sv => Seq(
      scalatest % "test"
    ) }
  )

  lazy val smd = Project(
    id =        "smd",
    base =      file("."),
    settings =  baseSettings,
    aggregate = Seq(core)
  )
  
  lazy val core = Project(
    id =       "smd-core",
    base =     file("smd"),
    settings = baseSettings ++ Seq()
  )
}
