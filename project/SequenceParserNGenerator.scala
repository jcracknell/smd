import sbt._

class SequenceParserNGenerator(n: Int) extends FileGenerator(_/"smd"/"parsing"/s"SequenceParser$n.scala") {
  import SequenceParserNGenerator._

  def content: String =
    s"""
    |package smd
    |package parsing
    |
    |${autoGenerated}
    |
    |/** Parses $n expressions, providing strongly-typed results.
    |  * ${(1 to n).map({ i => s"""
    |  * @param  p${i.toString.padTo(2, ' ')} the ${ordinal(i)} parser in the sequence.
    |  * @tparam T${i.toString.padTo(2, ' ')} the product type of the ${ordinal(i)} parser in the sequence.""".stripMargin }).mkString("")}
    |  */
    |case class ${className(n)}[${lst("+T", 1 to n)}](
    |  ${(1 to n).map(i => s"p$i: Parser[T$i]").mkString(", ")}
    |) extends SequenceParserLike[(${lst("T", 1 to n)})]
    |{
    |  lazy val sequence: IndexedSeq[Parser[Any]] = IndexedSeq(${lst("p", 1 to n)})
    |
    |  def parse(context: ParsingContext): ParsingResult[(${lst("T", 1 to n)})] = {
    |    val rb = context.resultBuilder
    |
    |    ${(1 to n).map({ i => s"""
    |    val r$i = p$i.parse(context)
    |    if(r$i.failed) return rb.failure
    |    """.trim.stripMargin }).mkString("\n").trim}
    |
    |    rb.success((${lst("r", 1 to n, ".product")}))
    |  }
    |}
    """.trim.stripMargin
}

object SequenceParserNGenerator {
  val MaxN: Int = 16 

  def className(x: Int) = s"SequenceParser$x"
  def qualifiedClassName(x: Int) = s"smd.parsing.${className(x)}"

  def ordinal(x: Int) = 
    x + IndexedSeq("th","st","nd","rd","th")(if(10 <= x && x <= 19) 4 else math.min(4, x % 10))

  def lst(prefix: String, is: Iterable[Int], suffix: String = ""): String = is.map(prefix+_+suffix).mkString(", ")
}
