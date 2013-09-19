package smd
package grammar

class InlineProductionsSpec extends ProductionSpec {
  import markdown._

  def subject = Grammar.inline

  describe("AutoLink") {
    shouldParse("<https://github.com>") as AutoLink("https://github.com")
    shouldParse("<mailto:john.doe@gmail.com>") as AutoLink("mailto:john.doe@gmail.com")
    shouldParse("<mailto:john.doe@gmail.com?subject=Where+is+my+money+you+bastard>") as AutoLink("mailto:john.doe@gmail.com?subject=Where+is+my+money+you+bastard")
  }
  describe("Code") {
    shouldParse("`a`") as Code("a")
    shouldParse("````````````````a````````````````") as Code("a")
    shouldParse("`//comment`") as Code("//comment")
  }
  describe("Emphasis") {
    shouldParse("*a*") as Emphasis(Seq(Text("a")))
    shouldParse("***a**b*") as Emphasis(Seq(Strong(Seq(Text("a"))), Text("b")))
  }
  describe("Link") {
    shouldParse("[foo](baz)") as Link(Seq(Text("foo")), None, Seq(expression.IriLiteral("baz")))
    shouldParse("[foo][bar]") as Link(Seq(Text("foo")), Some(ReferenceId("bar")), Seq())
    shouldParse("[foo][bar](baz)") as Link(Seq(Text("foo")), Some(ReferenceId("bar")), Seq(expression.IriLiteral("baz")))
    shouldParse("[Slashdot: News for nerds](http://www.slashdot.org)") as Link(
      Seq(Text("Slashdot:"), Space(), Text("News"), Space(), Text("for"), Space(), Text("nerds")),
      None,
      Seq(expression.IriLiteral("http://www.slashdot.org"))
    )
  }
  describe("Quoted") {
    shouldParse("''") as Quoted(Seq(), Quoted.QuoteKind.Single)
    shouldParse("\"\"") as Quoted(Seq(), Quoted.QuoteKind.Double)
    shouldParse("'a'") as Quoted(Seq(Text("a")), Quoted.QuoteKind.Single)
    shouldParse("\"a\"") as Quoted(Seq(Text("a")), Quoted.QuoteKind.Double)
  }
  describe("Strong") {
    shouldParse("**a**") as Strong(Seq(Text("a")))
    shouldParse("***a***") as Strong(Seq(Emphasis(Seq(Text("a")))))
    shouldParse("***a*b**") as Strong(Seq(Emphasis(Seq(Text("a"))), Text("b")))
  }
  describe("Text") {
    shouldParse("pelican") as Text("pelican")
    shouldParse("pelican's") as Text("pelican's")
  }
}
