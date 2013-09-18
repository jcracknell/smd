import sbt._

class SequenceParserNGenerator(n: Int) extends FileGenerator(_/"smd"/"parsing"/s"SequenceParser$n.scala") {
  import SequenceParserNGenerator._

  private def className(x: Int = n) = s"SequenceParser$x"
  private def qualifiedClassName(x: Int = n) = s"smd.parsing.${className(x)}"

  private def ordinal(x: Int) = {
    x + IndexedSeq("th","st","nd","rd","th")(if(10 <= x && x <= 19) 4 else math.min(4, x % 10))
  }

  private def lst(prefix: String, is: Iterable[Int], suffix: String = ""): String = is.map(prefix+_+suffix).mkString(", ")

  def content: String =
    s"""
    |package smd
    |package parsing
    |
    |// THIS FILE IS AUTOMATICALLY GENERATED
    |// Generated at: $timeStamp
    |
    |/** Parses $n expressions, providing strongly-typed results.
    |  * ${(1 to n).map({ i => s"""
    |  * @param  p${i.toString.padTo(2, ' ')} the ${ordinal(i)} parser in the sequence.
    |  * @tparam T${i.toString.padTo(2, ' ')} the product type of the ${ordinal(i)} parser in the sequence.""".stripMargin }).mkString("")}
    |  */
    |case class ${className()}[${lst("+T", 1 to n)}](
    |  ${(1 to n).map(i => s"p$i: Parser[T$i]").mkString(", ")}
    |) extends AnyRef with Parser[(${lst("T", 1 to n)})]
    |{
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
    |
    |object SequenceParser$n {
    |$sequencingHeuristic
    |${(2 to MaxN-n).map({ i => s"""
    |
    |  /** Implicit [[smd.parsing.SequencingHeuristic]] which describes how to combine an [[${qualifiedClassName(n)}]]
    |    * on the left-hand side with an [[${qualifiedClassName(i)}]] to its right. */
    |  implicit def sequencingHeuristic_${n}_${i}[${lst("L", 1 to n)}, ${lst("R", 1 to i)}]: SequencingHeuristic[
    |    /*  left: */ ${className(n)}[${lst("L", 1 to n)}],
    |    /* right: */ ${className(i)}[${lst("R", 1 to i)}],
    |    /*  dest: */ SequenceParser${n+i}[${lst("L", 1 to n)}, ${lst("R", 1 to i)}]
    |  ] =
    |    SequencingHeuristic.create((l, r) => ${className(n+i)}(${lst("l.p", 1 to n)}, ${lst("r.p", 1 to i)}))
    |""".trim.stripMargin }).mkString("")}
    |}
    """.trim.stripMargin

  def sequencingHeuristic: String =
    if(n == MaxN) s"""
    |  implicit def sequencingHeuristic[${lst("L", 1 to n)}, R]: SequencingHeuristic[
    |    /*  left: */ ${className(n)}[${lst("L", 1 to n)}],
    |    /* right: */ Parser[R],
    |    /*  dest: */ SequenceParser
    |  ] =
    |    SequencingHeuristic.create((l, r) => SequenceParser(${lst("l.p", 1 to n)}, r))
    """.trim.stripMargin
    else s"""
    |  /** Implicit [[smd.parsing.SequencingHeuristic]] which describes how to combine an [[${qualifiedClassName()}]]
    |    * and an [[smd.parsing.Parser]] into an [[${qualifiedClassName(n+1)}]]. */
    |  implicit def sequencingHeuristic[${lst("L", 1 to n)}, R]: SequencingHeuristic[
    |    /*  left: */ ${className(n)}[${lst("L", 1 to n)}],
    |    /* right: */ Parser[R],
    |    /*  dest: */ ${className(n+1)}[${lst("L", 1 to n)}, R]
    |  ] =
    |    SequencingHeuristic.create((l, r) => ${className(n+1)}(${lst("l.p", 1 to n)}, r))
    """.trim.stripMargin
}

object SequenceParserNGenerator {
  val MaxN: Int = 16 
}
