package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class PrimaryExpressionProductionsSpec extends ParsingScenarios {
  import Grammar.primaryExpression

  parsing("[]") as primaryExpression should produce (ArrayLiteral(Seq()))

  parsing("['a']") as primaryExpression should produce (ArrayLiteral(Seq(Some(StringLiteral("a")))))

  parsing("['a',42]") as primaryExpression should produce (ArrayLiteral(Seq(Some(StringLiteral("a")), Some(NumericLiteral(42d)))))

  parsing("[,]") as primaryExpression should produce (ArrayLiteral(Seq()))

  parsing("[,'a']") as primaryExpression should produce (ArrayLiteral(Seq(None, Some(StringLiteral("a")))))

  parsing("[,,'a','b',,,,]") as primaryExpression should produce (
    ArrayLiteral(Seq(
      None,
      None,
      Some(StringLiteral("a")),
      Some(StringLiteral("b"))
    ))
  )

  parsing("{}") as primaryExpression should produce (ObjectLiteral(Seq()))

  parsing("{'a'=42}") as primaryExpression should produce (ObjectLiteral(Seq("a" -> NumericLiteral(42d))))

  parsing("(@a || true)") as primaryExpression should produce ( LogicalOr(Identifier("a"), BooleanLiteral(true)))

  describe("block literals") {
    parsing("@<{text}>") as primaryExpression should produce (
      BlockLiteral(Seq(
        Paragraph(SourceRange.Value(3,7), Seq(
          Text(SourceRange.Value(3,7), "text")
        ))
      ))
    )
    parsing("@<{text}>") as primaryExpression should consume ("@<{text}>")
    parsing("@<{{{text}}}>") as primaryExpression should produce (
      BlockLiteral(Seq(
        Paragraph(SourceRange.Value(5,9), Seq(
          Text(SourceRange.Value(5,9), "text")
        ))
      ))
    )
    parsing("@<{}>") as primaryExpression should produce (BlockLiteral(Seq()))
    parsing("""
    |@<{  * item1
    |    * item2
    |}>
    """) as primaryExpression should produce (
      BlockLiteral(Seq(
        TightUnorderedList(SourceRange.Unknown, Seq(
          TightUnorderedList.Item(SourceRange.Unknown,
            Seq(Text(SourceRange.Unknown, "item1")),
            Seq(
              TightUnorderedList(SourceRange.Unknown, Seq(
                TightUnorderedList.Item(SourceRange.Unknown,
                  Seq(Text(SourceRange.Unknown, "item2"))
                )
              ))
            )
          )
        ))
      ))
    )
    parsing("""
    |@<{  * item1
    | |  * item2
    |}>
    """) as primaryExpression should produce (
      BlockLiteral(Seq(
        TightUnorderedList(SourceRange.Unknown, Seq(
          TightUnorderedList.Item(SourceRange.Unknown,
            Seq(Text(SourceRange.Unknown, "item1"))
          ),
          TightUnorderedList.Item(SourceRange.Unknown,
            Seq(Text(SourceRange.Unknown, "item2"))
          )
        ))
      ))
    )
    parsing("""
    |@<{ line1
    | | line2
    |}>
    """) as primaryExpression should produce (
      BlockLiteral(Seq(
        Paragraph(SourceRange.Unknown, Seq(
          Text(SourceRange.Unknown, "line1"),
          Space(SourceRange.Unknown),
          Text(SourceRange.Unknown, "line2")
        ))
      ))
    )
  }

  describe("inline literals") {
    parsing("@<[text]>") as primaryExpression should produce (
      InlineLiteral(Seq(Text(SourceRange.Unknown, "text")))
    )

    parsing("@<[text]>") as primaryExpression should consume ("@<[text]>")

    parsing("@<[[[text]]]>") as primaryExpression should produce (
      InlineLiteral(Seq(Text(SourceRange.Unknown, "text")))
    )

    parsing("@<[]>") as primaryExpression should produce (InlineLiteral(Seq()))

    parsing("""
    |@<[
    |  a b
    |]>
    """) as primaryExpression should produce (
      InlineLiteral(Seq(
        Text(SourceRange.Unknown, "a"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "b")
      ))
    )

    parsing("""
    |@<[ // comment ]>
    | text
    |]>
    """) as primaryExpression should produce (
      InlineLiteral(Seq(Text(SourceRange.Unknown, "text")))
    )
  }
}
