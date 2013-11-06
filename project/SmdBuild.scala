import sbt._
import Keys._

object SmdBuild extends Build {
  val generateSources = TaskKey[Unit]("generate-sources", "Generate dynamically generated source files used by the project.")
  val generateResources = TaskKey[Unit]("generate-resources", "Generate dynamically generate resource files used by the project.")

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
        "org.scalatest" %% "scalatest" % "latest.release" % "test"
      )
    }
  )

  lazy val `smd` = Project(
    id =           "smd",
    base =         file("."),
    settings =     baseSettings,
    aggregate =    Seq(`smd.core`, `smd.parsing`)
  )

  lazy val `smd.core` =  Project(
    id =           "core",
    base =         file("core"),
    dependencies = Seq(`smd.parsing` % "compile->compile;test->test"),
    settings = baseSettings ++ Seq(
      generateResources <<= resourceDirectory in Compile map { (baseDir: File) =>
        NamedEntitiesGenerator.generate(baseDir)
      }
    )
  )

  lazy val `smd.parsing` =  Project(
    id =         "parsing",
    base =       file("parsing"),
    settings = baseSettings ++ Seq(
      generateSources <<= scalaSource in Compile map { (baseDir: File) =>
        val generators = Seq(
          new SequencingHeuristicsGenerator,
          new ImplicitParserOpsGenerator
        ) ++ (
          (2 to SequenceParserNGenerator.MaxN).map(new SequenceParserNGenerator(_)).toList
        )

        generators foreach { _.generate(baseDir) }
      }
    )
  )
}
