package smd
package grammar

import smd.dom
import smd.dom._
import smd.parsing.ParsingScenarios

class InlineProductionsSpec extends ParsingScenarios {
  import Grammar.inline

  describe("Attribute") {
    parsing("{ b = c }") as inline should produce (Attributes(SourceRange.Unknown, Seq("b" -> IriLiteral("c"))))

    describe("space handling") {
      parsing("a{ b = c }d") as inline.* should produce (Seq(
        Text(SourceRange.Unknown, "a"), Attributes(SourceRange.Unknown, Seq("b" -> IriLiteral("c"))), Text(SourceRange.Unknown, "d")
      ))
      parsing("a { b = c }d") as inline.* should produce (Seq(
        Text(SourceRange.Unknown, "a"), Space(SourceRange.Unknown), Attributes(SourceRange.Unknown, Seq("b" -> IriLiteral("c"))), Text(SourceRange.Unknown, "d")
      ))
      parsing("a{ b = c } d") as inline.* should produce (Seq(
        Text(SourceRange.Unknown, "a"), Attributes(SourceRange.Unknown, Seq("b" -> IriLiteral("c"))), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "d")
      ))
      parsing("a { b = c } d") as inline.* should produce (Seq(
        Text(SourceRange.Unknown, "a"), Attributes(SourceRange.Unknown, Seq("b" -> IriLiteral("c"))), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "d")
      ))
    }
    describe("selector-like attribute syntax") {
      parsing("{#id}") as inline should produce (Attributes(SourceRange.Unknown, Seq("id" -> StringLiteral("id"))))
      parsing("{.class}") as inline should produce (Attributes(SourceRange.Unknown, Seq("class" -> StringLiteral("class"))))
      parsing("{#id.class}") as inline should produce (Attributes(SourceRange.Unknown, Seq(
        "id"    -> StringLiteral("id"),
        "class" -> StringLiteral("class")
      )))
      parsing("{.class#id}") as inline should produce (Attributes(SourceRange.Unknown, Seq(
        "class" -> StringLiteral("class"),
        "id"    -> StringLiteral("id")
      )))
      parsing("{#id .class}") as inline should produce (Attributes(SourceRange.Unknown, Seq(
        "id"    -> StringLiteral("id"),
        "class" -> StringLiteral("class")
      )))
      parsing("{ #id1 .class1.class2 #id2#id3 #id4.class3 .class4#id5 }") as inline should produce (Attributes(SourceRange.Unknown, Seq(
        "id"    -> StringLiteral("id1"),
        "class" -> StringLiteral("class1"),
        "class" -> StringLiteral("class2"),
        "id"    -> StringLiteral("id2"),
        "id"    -> StringLiteral("id3"),
        "id"    -> StringLiteral("id4"),
        "class" -> StringLiteral("class3"),
        "class" -> StringLiteral("class4"),
        "id"    -> StringLiteral("id5")
      )))
      parsing("{ foo = bar, #id1, fizz = buzz, .class1 }") as inline should produce (Attributes(SourceRange.Unknown, Seq(
        "foo"   -> IriLiteral("bar"),
        "id"    -> StringLiteral("id1"),
        "fizz"  -> IriLiteral("buzz"),
        "class" -> StringLiteral("class1")
      )))
    }
  }
  describe("AutoLink") {
    parsing("<https://github.com>") as inline should produce (AutoLink(SourceRange.Unknown, "https://github.com"))
    parsing("<mailto:john.doe@gmail.com>") as inline should produce (AutoLink(SourceRange.Unknown, "mailto:john.doe@gmail.com"))
    parsing("<mailto:john.doe@gmail.com?subject=Where+is+my+money+you+bastard>") as inline should produce (
      AutoLink(SourceRange.Unknown, "mailto:john.doe@gmail.com?subject=Where+is+my+money+you+bastard")
    )
  }
  describe("Code") {
    parsing("`a`") as inline should produce (Code(SourceRange.Unknown, "a"))
    parsing("````````````````a````````````````") as inline should produce (Code(SourceRange.Unknown, "a"))
    parsing("`//comment`") as inline should produce (Code(SourceRange.Unknown, "//comment"))
    parsing("`` ` ``") as inline should produce (Code(SourceRange.Unknown, "`"))
    parsing("` foo `") as inline should produce (Code(SourceRange.Unknown, "foo"))
    parsing("`` `println()` ``") as inline should produce(Code(SourceRange.Unknown, "`println()`"))
  }
  describe("Emphasis") {
    parsing("*a*") as inline should produce (Emphasis(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "a"))))
    parsing("***a**b*") as inline should produce (Emphasis(SourceRange.Unknown, Seq(Strong(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "a"))), Text(SourceRange.Unknown, "b"))))
  }
  describe("LineBreak") {
    parsing("""
    |  \
    | // content required
    """) as inline should produce (LineBreak(SourceRange.Unknown))
  }
  describe("Entity") {
    parsing("""\#233""") as inline should produce (Entity(SourceRange(0, 5), "é".map(_.toInt)))
    parsing("""\#233;""") as inline should produce (Entity(SourceRange(0, 6), "é".map(_.toInt)))
    parsing("""\eacute;""") as inline should produce (Entity(SourceRange(0, 8), "é".map(_.toInt)))
    parsing("""\$foo()""") as inline should produce (Entity(SourceRange(0, 2), Seq('$'.toInt)))
    parsing("""\n""") as inline should produce (Symbol(SourceRange(0, 1), "\\"))
    parsing("""\t""") as inline should produce (Symbol(SourceRange(0, 1), "\\"))
  }
  describe("InlineExpression") {
    parsing("""@1""") as inline should produce (InlineExpression(SourceRange.Unknown, NumericLiteral(1d)))
    parsing("""@-1""") as inline should produce (Symbol(SourceRange.Unknown, "@"))
    parsing("""@(-1)""") as inline should produce (InlineExpression(SourceRange.Unknown, Negative(NumericLiteral(1d))))
    parsing("""@1 + 2""") as inline should produce (InlineExpression(SourceRange.Unknown, NumericLiteral(1d)))
    parsing("""@(1 + 2)""") as inline should produce (InlineExpression(SourceRange.Unknown, Addition(NumericLiteral(1d), NumericLiteral(2d))))
    parsing("@foo.bar") as inline should produce (InlineExpression(SourceRange(0, 8), Member(Identifier("foo"), "bar")))
    parsing("@foo. bar") as inline should produce (InlineExpression(SourceRange(0, 4), Identifier("foo")))
    parsing("@foo.bar;") as inline should produce (InlineExpression(SourceRange(0, 9), Member(Identifier("foo"), "bar")))
    parsing("@img(src)") as inline should produce (InlineExpression(SourceRange(0, 9), Application(Identifier("img"), Seq(IriLiteral("src")))))
    parsing("@img (src)") as inline should produce (InlineExpression(SourceRange(0, 4), Identifier("img")))
    parsing("""@{if(true) 1 else 2}""") as inline should produce (
      InlineExpression(SourceRange.Unknown, 
        Conditional(BooleanLiteral(true), NumericLiteral(1d), Some(NumericLiteral(2d)))
      )
    )
    parsing("""@{if(true) 1} elsie""") as inline should produce (
      InlineExpression(SourceRange.Unknown, 
        Conditional(BooleanLiteral(true), NumericLiteral(1d), None)
      )
    )
    parsing("""
    |@box(@<{
    | | # heading
    | | 
    | | content
    |}>)
    """) as inline should produce (
      InlineExpression(SourceRange.Unknown,
        Application(Identifier("box"), Seq(
          BlockLiteral(Seq(
            Heading(SourceRange.Unknown, 1, Seq(
              Text(SourceRange.Unknown, "heading")
            )),
            Paragraph(SourceRange.Unknown, Seq(
              Text(SourceRange.Unknown, "content")
            ))
          ))
        ))
      )
    )
  }
  describe("Link") {
    parsing("[foo](baz)") as inline should produce (Link(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "foo")), None, Seq(IriLiteral("baz"))))
    parsing("[foo][bar]") as inline should produce (Link(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "foo")), Some(ReferenceId("bar")), Seq()))
    parsing("[foo][bar](baz)") as inline should produce (
      Link(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "foo")), Some(ReferenceId("bar")), Seq(IriLiteral("baz")))
    )
    parsing("[Slashdot: News for nerds](http://www.slashdot.org)") as inline should produce (
      Link(SourceRange.Unknown, 
        Seq(Text(SourceRange.Unknown, "Slashdot:"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "News"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "for"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "nerds")),
        None,
        Seq(IriLiteral("http://www.slashdot.org"))
      )
    )
  }
  describe("Quoted") {
    parsing("''") as inline should produce (Quoted(SourceRange.Unknown, Seq(), Quoted.QuoteKind.Single))
    parsing("\"\"") as inline should produce (Quoted(SourceRange.Unknown, Seq(), Quoted.QuoteKind.Double))
    parsing("'a'") as inline should produce (Quoted(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "a")), Quoted.QuoteKind.Single))
    parsing("\"a\"") as inline should produce (Quoted(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "a")), Quoted.QuoteKind.Double))
  }
  describe("Strong") {
    parsing("**a**") as inline should produce (Strong(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "a"))))
    parsing("***a***") as inline should produce (Strong(SourceRange.Unknown, Seq(Emphasis(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "a"))))))
    parsing("***a*b**") as inline should produce (Strong(SourceRange.Unknown, Seq(Emphasis(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "a"))), Text(SourceRange.Unknown, "b"))))
  }
  describe("Subscript") {
    parsing("""~1~""") as inline should produce (Subscript(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "1"))))
    parsing("""~1 2~""") as inline should produce (Symbol(SourceRange.Unknown, "~"))
  }
  describe("Superscript") {
    parsing("""^1^""") as inline should produce (Superscript(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "1"))))
    parsing("""^1 2^""") as inline should produce (Symbol(SourceRange.Unknown, "^"))
  }
  describe("Text") {
    parsing("pelican") as inline should produce (Text(SourceRange.Unknown, "pelican"))
    parsing("pelican's") as inline should produce (Text(SourceRange.Unknown, "pelican's"))
  }
  describe("precedence") {
    parsing("**@<[text**]>") as inline should produce (Symbol(SourceRange.Unknown, "*"))
    parsing("**[label**](url)") as inline should produce (Symbol(SourceRange.Unknown, "*"))
    parsing("*@<[text*]>") as inline should produce (Symbol(SourceRange.Unknown, "*"))
    parsing("*[label*](url)") as inline should produce (Symbol(SourceRange.Unknown, "*"))
    parsing("@<[*text]>*]>") as inline should produce (
      InlineExpression(SourceRange.Unknown, InlineLiteral(Seq(
        Symbol(SourceRange.Unknown, "*"), Text(SourceRange.Unknown, "text") 
      )))
    )
    parsing("[*label](url)*](url)") as inline should produce (
      Link(SourceRange.Unknown,
        Seq(Symbol(SourceRange.Unknown, "*"), Text(SourceRange.Unknown, "label")),
        None,
        Seq(IriLiteral("url"))
      )
    )
  }
}
