package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class PrimaryExpressionProductionsSpec extends ParsingScenarios {
  import Grammar.primaryExpression

  describe("null literal") {
    parsing("null") as primaryExpression should produce (NullLiteral())
  }
  describe("numeric literals") {
    parsing("0")      as primaryExpression should produce (NumericLiteral(0d))
    parsing(".0123")  as primaryExpression should produce (NumericLiteral(0.0123d))
    parsing("12.345") as primaryExpression should produce (NumericLiteral(12.345d))
    parsing("12e1")   as primaryExpression should produce (NumericLiteral(120d))
    parsing("12E2")   as primaryExpression should produce (NumericLiteral(1200d))
    parsing("0x1234") as primaryExpression should produce (NumericLiteral(0x1234.toDouble))
  }
  describe("string literals") {
    parsing("''")           as primaryExpression should produce (StringLiteral(""))
    parsing("\"\"")         as primaryExpression should produce (StringLiteral(""))
    parsing("``")           as primaryExpression should reject
    parsing("'a'")          as primaryExpression should produce (StringLiteral("a"))
    parsing("\"a\"")        as primaryExpression should produce (StringLiteral("a"))
    parsing("`a`")          as primaryExpression should produce (VerbatimLiteral("a"))
    parsing("````````````a````````````") as primaryExpression should produce (VerbatimLiteral("a"))
    parsing("'\\0z'")       as primaryExpression should produce (StringLiteral("\\0z"))
    parsing("`\\0z`")       as primaryExpression should produce (VerbatimLiteral("\\0z"))
    parsing("'\\x00z'")     as primaryExpression should produce (StringLiteral("\u0000z"))
    parsing("`\\x00z`")     as primaryExpression should produce (VerbatimLiteral("\\x00z"))
    parsing("'\\x4a'")      as primaryExpression should produce (StringLiteral("J"))
    parsing("`\\x4z`")      as primaryExpression should produce (VerbatimLiteral("\\x4z"))
    parsing("'\\u0000z'")   as primaryExpression should produce (StringLiteral("\u0000z"))
    parsing("'\\x0000z'")   as primaryExpression should produce (StringLiteral("\u0000z"))
    parsing("'\\#u0000z'")  as primaryExpression should produce (StringLiteral("\u0000z"))
    parsing("'\\#x0000z'")  as primaryExpression should produce (StringLiteral("\u0000z"))
    parsing("'\\u0000;z'")  as primaryExpression should produce (StringLiteral("\u0000z"))
    parsing("'\\x0000;z'")  as primaryExpression should produce (StringLiteral("\u0000z"))
    parsing("`\\u0000z`")   as primaryExpression should produce (VerbatimLiteral("\\u0000z"))
    parsing("'\\u004a'")    as primaryExpression should produce (StringLiteral("J"))
    parsing("'\\#u4a'")     as primaryExpression should produce (StringLiteral("J"))
    parsing("'\\#u4a;'")    as primaryExpression should produce (StringLiteral("J"))
    parsing("'\\u4a;'")     as primaryExpression should produce (StringLiteral("J"))
    parsing("`\\u004z`")    as primaryExpression should produce (VerbatimLiteral("\\u004z"))
    parsing("'\\eacutez'")  as primaryExpression should produce (StringLiteral("\\eacutez"))
    parsing("'\\eacute;z'") as primaryExpression should produce (StringLiteral("éz"))
    parsing("`` `a` ``") as primaryExpression should produce (VerbatimLiteral("`a`"))
    parsing("` a `") as primaryExpression should produce (VerbatimLiteral("a"))
    parsing("`  a  `") as primaryExpression should produce (VerbatimLiteral(" a "))
    parsing("'a@{foo}b'") as primaryExpression should produce (Interpolation(Seq(StringLiteral("a"), IriLiteral("foo"), StringLiteral("b"))))
    parsing("'Hello @user!'") as primaryExpression should produce (Interpolation(Seq(StringLiteral("Hello "), Identifier("user"), StringLiteral("!"))))
    parsing("\"@foo\"") as primaryExpression should produce (Interpolation(Seq(Identifier("foo"))))
    parsing("'Hello @user.toUpperCase(), it is @date today.'") as primaryExpression should produce (Interpolation(Seq(
      StringLiteral("Hello "), Application(Member(Identifier("user"), "toUpperCase"), Seq()), StringLiteral(", it is "), Identifier("date"), StringLiteral(" today.")
    )))
  }
  describe("iri literals") {
    parsing("nullish") as primaryExpression should produce (IriLiteral("nullish"))
    parsing("trueish") as primaryExpression should produce (IriLiteral("trueish"))
    parsing("falsey") as primaryExpression should produce (IriLiteral("falsey"))
    parsing("http://www.google.com") as primaryExpression should produce (IriLiteral("http://www.google.com"))
    parsing("http://msdn.microsoft.com/en-us/library/a6td98xe(v=vs.71).aspx") as primaryExpression should produce (IriLiteral("http://msdn.microsoft.com/en-us/library/a6td98xe(v=vs.71).aspx"))
    //parsing("http://msdn.microsoft.com/en-us/library/a6td98xev=vs.71).aspx") as primaryExpression should produce (IriLiteral("http://msdn.microsoft.com/en-us/library/a6td98xev=vs.71"))
    parsing("http://bücher.de/") as primaryExpression should produce (IriLiteral("http://bücher.de/"))
    parsing("my:irn:scheme:24") as primaryExpression should produce (IriLiteral("my:irn:scheme:24"))
    parsing("http://www.cracknell.ca") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca"))
    parsing("http://www.cracknell.ca/") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca/"))
    parsing("http://www.cracknell.ca/some/path") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca/some/path"))
    parsing("http://www.cracknell.ca?q=bla") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca?q=bla"))
    parsing("http://www.cracknell.ca#fragment") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca#fragment"))
    parsing("http://www.cracknell.ca/some/path?q=bla") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca/some/path?q=bla"))
    parsing("http://www.cracknell.ca/some/path#fragment") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca/some/path#fragment"))
    parsing("http://www.cracknell.ca/some/path?q=bla#fragment") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca/some/path?q=bla#fragment"))
    parsing("www.cracknell.ca") as primaryExpression should produce (IriLiteral("www.cracknell.ca"))
    parsing("www.cracknell.ca/") as primaryExpression should produce (IriLiteral("www.cracknell.ca/"))
    parsing("www.cracknell.ca/some/path") as primaryExpression should produce (IriLiteral("www.cracknell.ca/some/path"))
    parsing("www.cracknell.ca?q=bla") as primaryExpression should produce (IriLiteral("www.cracknell.ca?q=bla"))
    parsing("www.cracknell.ca#fragment") as primaryExpression should produce (IriLiteral("www.cracknell.ca#fragment"))
    parsing("www.cracknell.ca/some/path?q=bla") as primaryExpression should produce (IriLiteral("www.cracknell.ca/some/path?q=bla"))
    parsing("www.cracknell.ca/some/path#fragment") as primaryExpression should produce (IriLiteral("www.cracknell.ca/some/path#fragment"))
    parsing("www.cracknell.ca/some/path?q=bla#fragment") as primaryExpression should produce (IriLiteral("www.cracknell.ca/some/path?q=bla#fragment"))
    parsing("resum\\eacute;.com") as primaryExpression should produce (IriLiteral("resumé.com"))
    parsing("james@www.cracknell.ca") as primaryExpression should produce (IriLiteral("james@www.cracknell.ca"))
    parsing("www.cracknell.ca:80") as primaryExpression should produce (IriLiteral("www.cracknell.ca:80"))
    parsing("james@www.cracknell.ca:80") as primaryExpression should produce (IriLiteral("james@www.cracknell.ca:80"))
    parsing("123.234.210.10") as primaryExpression should produce (IriLiteral("123.234.210.10"))
    parsing("james@123.234.210.10") as primaryExpression should produce (IriLiteral("james@123.234.210.10"))
    parsing("123.234.210.10:80") as primaryExpression should produce (IriLiteral("123.234.210.10:80"))
    parsing("james@123.234.210.10:80") as primaryExpression should produce (IriLiteral("james@123.234.210.10:80"))
    parsing("[FEDC:BA98:7654:3210:0123:4567:89AB:CDEF]") as primaryExpression should produce (IriLiteral("[FEDC:BA98:7654:3210:0123:4567:89AB:CDEF]"))
    parsing("james@[FEDC:BA98:7654:3210:0123:4567:89AB:CDEF]") as primaryExpression should produce (IriLiteral("james@[FEDC:BA98:7654:3210:0123:4567:89AB:CDEF]"))
    parsing("[FEDC:BA98:7654:3210:0123:4567:89AB:CDEF]:80") as primaryExpression should produce (IriLiteral("[FEDC:BA98:7654:3210:0123:4567:89AB:CDEF]:80"))
    parsing("james@[FEDC:BA98:7654:3210:0123:4567:89AB:CDEF]:80") as primaryExpression should produce (IriLiteral("james@[FEDC:BA98:7654:3210:0123:4567:89AB:CDEF]:80"))
    parsing("/") as primaryExpression should produce (IriLiteral("/"))
    parsing("/some/path") as primaryExpression should produce (IriLiteral("/some/path"))
    parsing("/some/path/") as primaryExpression should produce (IriLiteral("/some/path/"))
    parsing("/?q=bla") as primaryExpression should produce (IriLiteral("/?q=bla"))
    parsing("/#fragment") as primaryExpression should produce (IriLiteral("/#fragment"))
    parsing("/some/path?q=bla") as primaryExpression should produce (IriLiteral("/some/path?q=bla"))
    parsing("/some/path#fragment") as primaryExpression should produce (IriLiteral("/some/path#fragment"))
    parsing("/some/path?q=bla#fragment") as primaryExpression should produce (IriLiteral("/some/path?q=bla#fragment"))
    parsing("path") as primaryExpression should produce (IriLiteral("path"))
    parsing("some/path") as primaryExpression should produce (IriLiteral("some/path"))
    parsing("?q=bla") as primaryExpression should produce (IriLiteral("?q=bla"))
    parsing("#fragment") as primaryExpression should produce (IriLiteral("#fragment"))
    parsing("some/path?q=bla") as primaryExpression should produce (IriLiteral("some/path?q=bla"))
    parsing("some/path#fragment") as primaryExpression should produce (IriLiteral("some/path#fragment"))
    parsing("some/path?q=bla#fragment") as primaryExpression should produce (IriLiteral("some/path?q=bla#fragment"))
    //parsing("http://www.cracknell.ca/a,") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca/a,"))
    parsing("http://www.cracknell.ca/a,a") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca/a,a"))
    //parsing("http://www.cracknell.ca?q=a,") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca?q=a,"))
    parsing("http://www.cracknell.ca?q=a,a") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca?q=a,a"))
    //parsing("http://www.cracknell.ca/a:") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca/a:"))
    parsing("http://www.cracknell.ca/a:a") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca/a:a"))
    //parsing("http://www.cracknell.ca?q=a:") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca?q=a:"))
    parsing("http://www.cracknell.ca?q=a:a") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca?q=a:a"))
    //parsing("http://www.cracknell.ca/a;") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca/a;"))
    parsing("http://www.cracknell.ca/a;a") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca/a;a"))
    //parsing("http://www.cracknell.ca?q=a;") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca?q=a;"))
    parsing("http://www.cracknell.ca?q=a;a") as primaryExpression should produce (IriLiteral("http://www.cracknell.ca?q=a;a"))
    parsing("abcd:abcd:abcd:abcd:abcd:abcd:abcd:abcd") as primaryExpression should produce (IriLiteral("abcd:abcd:abcd:abcd:abcd:abcd:abcd:abcd"))
    parsing("123.123.123.123") as primaryExpression should produce (IriLiteral("123.123.123.123"))
    parsing("1.1.1.1") as primaryExpression should produce (IriLiteral("1.1.1.1"))
    parsing("0.0.0.0") as primaryExpression should produce (IriLiteral("0.0.0.0"))
    parsing("foo,bar") as primaryExpression should produce (IriLiteral("foo,bar"))

    // RFC 2732 example IPv6 urls
    parsing("http://[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]:80/index.html") as primaryExpression should produce (IriLiteral("http://[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]:80/index.html"))
    parsing("http://[1080:0:0:0:8:800:200C:417A]/index.html") as primaryExpression should produce (IriLiteral("http://[1080:0:0:0:8:800:200C:417A]/index.html"))
    parsing("http://[3ffe:2a00:100:7031::1]") as primaryExpression should produce (IriLiteral("http://[3ffe:2a00:100:7031::1]"))
    parsing("http://[1080::8:800:200C:417A]/foo") as primaryExpression should produce (IriLiteral("http://[1080::8:800:200C:417A]/foo"))
    parsing("http://[::192.9.5.5]/ipng") as primaryExpression should produce (IriLiteral("http://[::192.9.5.5]/ipng"))
    parsing("http://[::FFFF:129.144.52.38]:80/index.html") as primaryExpression should produce (IriLiteral("http://[::FFFF:129.144.52.38]:80/index.html"))
    parsing("http://[2010:836B:4179::836B:4179]") as primaryExpression should produce (IriLiteral("http://[2010:836B:4179::836B:4179]"))
    // RFC 2732 example IPv6 urls with no scheme
    parsing("[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]:80/index.html") as primaryExpression should produce (IriLiteral("[FEDC:BA98:7654:3210:FEDC:BA98:7654:3210]:80/index.html"))
    parsing("[1080:0:0:0:8:800:200C:417A]/index.html") as primaryExpression should produce (IriLiteral("[1080:0:0:0:8:800:200C:417A]/index.html"))
    parsing("[3ffe:2a00:100:7031::1]") as primaryExpression should produce (IriLiteral("[3ffe:2a00:100:7031::1]"))
    parsing("[1080::8:800:200C:417A]/foo") as primaryExpression should produce (IriLiteral("[1080::8:800:200C:417A]/foo"))
    parsing("[::192.9.5.5]/ipng") as primaryExpression should produce (IriLiteral("[::192.9.5.5]/ipng"))
    parsing("[::FFFF:129.144.52.38]:80/index.html") as primaryExpression should produce (IriLiteral("[::FFFF:129.144.52.38]:80/index.html"))
    parsing("[2010:836B:4179::836B:4179]") as primaryExpression should produce (IriLiteral("[2010:836B:4179::836B:4179]"))
  }

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
