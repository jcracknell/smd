package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class InlineProductionsSpec extends ParsingScenarios {
  import Grammar.inline

  describe("AutoLink") {
    parsing("<https://github.com>") as inline should produce (AutoLink("https://github.com"))
    parsing("<mailto:john.doe@gmail.com>") as inline should produce (AutoLink("mailto:john.doe@gmail.com"))
    parsing("<mailto:john.doe@gmail.com?subject=Where+is+my+money+you+bastard>") as inline should produce (
      AutoLink("mailto:john.doe@gmail.com?subject=Where+is+my+money+you+bastard")
    )
  }
  describe("Code") {
    parsing("`a`") as inline should produce (Code("a"))
    parsing("````````````````a````````````````") as inline should produce (Code("a"))
    parsing("`//comment`") as inline should produce (Code("//comment"))
  }
  describe("Emphasis") {
    parsing("*a*") as inline should produce (Emphasis(Seq(Text("a"))))
    parsing("***a**b*") as inline should produce (Emphasis(Seq(Strong(Seq(Text("a"))), Text("b"))))
  }
  describe("LineBreak") {
    parsing("""
    |  \
    | // content required
    """) as inline should produce (LineBreak())
  }
  describe("Link") {
    parsing("[foo](baz)") as inline should produce (Link(Seq(Text("foo")), None, Seq(IriLiteral("baz"))))
    parsing("[foo][bar]") as inline should produce (Link(Seq(Text("foo")), Some(ReferenceId("bar")), Seq()))
    parsing("[foo][bar](baz)") as inline should produce (
      Link(Seq(Text("foo")), Some(ReferenceId("bar")), Seq(IriLiteral("baz")))
    )
    parsing("[Slashdot: News for nerds](http://www.slashdot.org)") as inline should produce (
      Link(
        Seq(Text("Slashdot:"), Space(), Text("News"), Space(), Text("for"), Space(), Text("nerds")),
        None,
        Seq(IriLiteral("http://www.slashdot.org"))
      )
    )
  }
  describe("Quoted") {
    parsing("''") as inline should produce (Quoted(Seq(), Quoted.QuoteKind.Single))
    parsing("\"\"") as inline should produce (Quoted(Seq(), Quoted.QuoteKind.Double))
    parsing("'a'") as inline should produce (Quoted(Seq(Text("a")), Quoted.QuoteKind.Single))
    parsing("\"a\"") as inline should produce (Quoted(Seq(Text("a")), Quoted.QuoteKind.Double))
  }
  describe("Strong") {
    parsing("**a**") as inline should produce (Strong(Seq(Text("a"))))
    parsing("***a***") as inline should produce (Strong(Seq(Emphasis(Seq(Text("a"))))))
    parsing("***a*b**") as inline should produce (Strong(Seq(Emphasis(Seq(Text("a"))), Text("b"))))
  }
  describe("Text") {
    parsing("pelican") as inline should produce (Text("pelican"))
    parsing("pelican's") as inline should produce (Text("pelican's"))
  }
}
