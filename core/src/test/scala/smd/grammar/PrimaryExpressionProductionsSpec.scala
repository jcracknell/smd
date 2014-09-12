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

  parsing("{'a':42}") as primaryExpression should produce (ObjectLiteral(Seq(StringLiteral("a") -> NumericLiteral(42d))))
  parsing("{@foo.bar: baz}") as primaryExpression should produce (
    ObjectLiteral(Seq(Member(Identifier("foo"), "bar") -> IriLiteral("baz")))
  )
  parsing("{@foo(bar): baz}") as primaryExpression should produce (
    ObjectLiteral(Seq(Application(Identifier("foo"), Seq(IriLiteral("bar"))) -> IriLiteral("baz")))
  )

  describe("selector-like property syntax") {
    parsing("{#id}") as primaryExpression should produce (ObjectLiteral(Seq(StringLiteral("id") -> StringLiteral("id"))))
    parsing("{.class}") as primaryExpression should produce (ObjectLiteral(Seq(StringLiteral("class") -> StringLiteral("class"))))
    parsing("{#id.class}") as primaryExpression should produce (ObjectLiteral(Seq(
      StringLiteral("id")    -> StringLiteral("id"),
      StringLiteral("class") -> StringLiteral("class")
    )))
    parsing("{.class#id}") as primaryExpression should produce (ObjectLiteral(Seq(
      StringLiteral("class") -> StringLiteral("class"),
      StringLiteral("id")    -> StringLiteral("id")
    )))
    parsing("{#id .class}") as primaryExpression should produce (ObjectLiteral(Seq(
      StringLiteral("id")    -> StringLiteral("id"),
      StringLiteral("class") -> StringLiteral("class")
    )))
    parsing("{ #id1 .class1.class2 #id2#id3 #id4.class3 .class4#id5 }") as primaryExpression should produce (ObjectLiteral(Seq(
      StringLiteral("id")    -> StringLiteral("id1"),
      StringLiteral("class") -> StringLiteral("class1"),
      StringLiteral("class") -> StringLiteral("class2"),
      StringLiteral("id")    -> StringLiteral("id2"),
      StringLiteral("id")    -> StringLiteral("id3"),
      StringLiteral("id")    -> StringLiteral("id4"),
      StringLiteral("class") -> StringLiteral("class3"),
      StringLiteral("class") -> StringLiteral("class4"),
      StringLiteral("id")    -> StringLiteral("id5")
    )))
    parsing("{ foo: bar, #id1, fizz: buzz, .class1 }") as primaryExpression should produce (ObjectLiteral(Seq(
      IriLiteral("foo")   -> IriLiteral("bar"),
      StringLiteral("id")    -> StringLiteral("id1"),
      IriLiteral("fizz")  -> IriLiteral("buzz"),
      StringLiteral("class") -> StringLiteral("class1")
    )))
    parsing("{ #id: .class .class: #id }") as primaryExpression should produce (
      ObjectLiteral(Seq(IriLiteral("#id") -> IriLiteral(".class"), IriLiteral(".class") -> IriLiteral("#id")))
    )
  }

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

    parsing("@<[**text]>**") as primaryExpression should produce (
      InlineLiteral(Seq(
        Symbol(SourceRange.Unknown, "*"), Symbol(SourceRange.Unknown, "*"), Text(SourceRange.Unknown, "text")
      ))
    )
  }
}
