import sbt._
import Keys._

object Dependencies {
  def scalatest = "org.scalatest" %% "scalatest" % "1.9+"
}

object SmdBuild extends Build {
  import Dependencies._

  val generateSources = TaskKey[Unit]("generate-sources", "Generate dynamically generated source files used by the project.")

  val baseSettings = Defaults.defaultSettings ++ Seq(
    version :=      "0.1",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      "-Xlint",
      "-Yno-adapted-args",
      "-Ywarn-all",
      "-Ywarn-dead-code"
    ),
    scalaVersion := "2.10.2",
    libraryDependencies <++= (scalaVersion) { sv => Seq(
      scalatest % "test"
    ) }
  )

  lazy val smd = Project(
    id =           "smd",
    base =         file("."),
    settings =     baseSettings,
    aggregate =    Seq(main, parsing)
  )

  lazy val main = Project(
    id =           "smd-main",
    base =         file("smd"),
    dependencies = Seq(parsing),
    settings =     baseSettings
  )

  lazy val parsing = {
    val generateSourcesTask = generateSources <<= scalaSource.in(Compile) map { (baseDir: File) =>
      val sequenceParserGenerators = (2 to SequenceParserNGenerator.MaxN) map { new SequenceParserNGenerator(_) } toList

      val generators = sequenceParserGenerators

      generators.foreach(_.generate(baseDir))
    }

    Project(
      id =         "smd-parsing",
      base =       file("smd.parsing"),
      settings =   baseSettings ++ Seq(generateSourcesTask)
    )
  }
}
