package smd
package grammar

class InlineProductionsSpec extends ProductionSpec {
  import markdown._

  def subject = Grammar.Inline

  describe("Code") {
    shouldParse("``") as Code("")
    shouldParse("`a`") as Code("a")
    shouldParse("````````````````````````````````") as Code("")
    shouldParse("````````````````a````````````````") as Code("a")
    shouldParse("`//comment`") as Code("//comment")
  }
  describe("Emphasis") {
    shouldParse("*a*") as Emphasis(Seq(Text("a")))
    shouldParse("***a**b*") as Emphasis(Seq(Strong(Seq(Text("a"))), Text("b")))
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
}
