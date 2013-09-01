import sbt._

abstract class FileGenerator(path: File => File) {
  def generate(base: File): Unit = {
    val target = path(base)

    println(s"Generating ${target.toString}")

    target.createNewFile()
    val writer = new java.io.PrintWriter(target.toString, "UTF-8")
    try { writer.write(content) } finally { writer.close() }
  }

  def content: String

  def timeStamp: String = {
    val df = new java.text.SimpleDateFormat("yyyy'-'MM'-'dd'T'HH':'mm':'ss'Z'")
    df.setTimeZone(java.util.TimeZone.getTimeZone("UTC"))
    df.format(new java.util.Date())
  }
}
