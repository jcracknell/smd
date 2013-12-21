import sbt._
import Keys._

object SmdBuild extends Build {
  val generateResources = TaskKey[Unit]("generate-resources", "Generate dynamically generate resource files used by the project.")
  val freemarker = TaskKey[Unit]("freemarker")

  val baseSettings = Defaults.defaultSettings ++ Seq(
    version :=      "0.1",
    scalaVersion := "2.10.3",
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
    },
    freemarker <<= (unmanagedSourceDirectories in Compile) map { (sources) =>
      sources.get foreach { sourceDir =>
        (sourceDir ** "*.fm").get foreach { templateFile =>
          val templatePath = IO.relativize(sourceDir, templateFile).get
          val outputFile = sourceDir/templatePath.stripSuffix(".fm")
          println(s"Processing ${templateFile} -> ${outputFile}")

          val cfg = new _root_.freemarker.template.Configuration()
          cfg.setDirectoryForTemplateLoading(sourceDir)
          cfg.setObjectWrapper(new _root_.freemarker.template.DefaultObjectWrapper())

          val template = cfg.getTemplate(templatePath)

          val ostream = new java.io.FileOutputStream(outputFile)
          try {
            val writer = new java.io.OutputStreamWriter(ostream, "UTF-8")
            try {
              template.process(new java.lang.Object, writer)
              writer.flush()
            } finally { writer.close() }
          } finally { ostream.close() }
        }
      }
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
    settings = baseSettings ++ Seq()
  )

  lazy val `smd.readme` = Project(
    id =           "readme",
    base =         file("readme"),
    dependencies = Seq(`smd.core`),
    settings =     baseSettings
  )
}
