import sbt._
import Keys._

object SmdBuild extends Build {
  val generateSources = TaskKey[Unit]("generate-sources", "Generate dynamically generated source files used by the project.")

  val baseSettings = Defaults.defaultSettings ++ Seq(
    version :=      "0.1",
    scalaVersion := "2.10.2",
    scalacOptions ++= Seq(
      "-deprecation",
      "-feature",
      //"-Xfatal-warnings",
      "-Xlint",
      "-Ywarn-adapted-args",
      "-Ywarn-dead-code",
      "-Ywarn-inaccessible",
      "-Ywarn-nullary-override",
      "-Ywarn-nullary-unit",
      "-Ywarn-numeric-widen"  
    ),
    libraryDependencies <++= (scalaVersion) { sv =>
      Seq(
        "org.scalatest" %% "scalatest" % "latest.snapshot" % "test"
      )
    }
  )

  lazy val all = Project(
    id =           "all",
    base =         file("."),
    settings =     baseSettings,
    aggregate =    Seq(`smd`, `smd.parsing`)
  )

  lazy val `smd` = Project(
    id =           "smd",
    base =         file("smd"),
    dependencies = Seq(`smd.parsing` % "compile->compile;test->test"),
    settings =     baseSettings
  )

  lazy val `smd.parsing` = {
    val generateSourcesTask = generateSources <<= scalaSource.in(Compile) map { (baseDir: File) =>
      (
        (2 to SequenceParserNGenerator.MaxN map { new SequenceParserNGenerator(_) } toList) :::
        new SequencingHeuristicsGenerator ::
        new ImplicitParserOpsGenerator :: Nil
      ) foreach(_.generate(baseDir))
    }

    Project(
      id =         "smd-parsing",
      base =       file("smd.parsing"),
      settings =   baseSettings ++ Seq(generateSourcesTask)
    )
  }
}
