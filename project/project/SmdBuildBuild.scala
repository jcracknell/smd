import sbt._
import Keys._

object SmdBuildBuild extends Build {
  lazy val default = Project(
    id = "smd-build",
    base = file("."),
    settings = Defaults.defaultSettings ++ Seq(
      libraryDependencies ++= Seq(
        "io.spray" %% "spray-json" % "latest.release",
        "org.freemarker" % "freemarker" % "latest.release"
      ),
      resolvers := Resolver withDefaultResolvers Seq(
        "spray" at "http://repo.spray.io/"
      )
    )
  )
}
