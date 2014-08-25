package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class HeadingSpec extends ParsingScenarios {
  import Grammar.heading

  parsing("""
  |# Heading 1
  """) as heading should produce (
    Heading(SourceRange.Unknown, 1, Seq(
      Text(SourceRange.Unknown, "Heading"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "1")
    ))
  )

  parsing("""
  |## Heading 2
  """) as heading should produce (
    Heading(SourceRange.Unknown, 2, Seq(
      Text(SourceRange.Unknown, "Heading"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "2")
    ))
  )

  parsing("""
  |/* comment */ ## Heading Text
  """) as heading should produce (
    Heading(SourceRange.Unknown, 2, Seq(
      Text(SourceRange.Unknown, "Heading"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "Text")
    ))
  )
  parsing("""
  |// comment
  |## Heading Text
  """) as heading should produce (
    Heading(SourceRange.Unknown, 2, Seq(
      Text(SourceRange.Unknown, "Heading"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "Text")
    ))
  )
}
