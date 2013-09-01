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

  lazy val core = {
    val generateSourcesTask = generateSources <<= scalaSource.in(Compile) map { (baseDir: File) =>
      val sequenceParserGenerators = (2 to 16) map { new SequenceParserNGenerator(_, 16) } toList

      val generators = sequenceParserGenerators

      generators.foreach(_.generate(baseDir))
    }

    Project(
      id =       "smd-core",
      base =     file("smd"),
      settings = baseSettings ++ Seq(generateSourcesTask)
    )
  }
}
