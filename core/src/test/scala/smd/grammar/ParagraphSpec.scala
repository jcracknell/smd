package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class ParagraphSpec extends ParsingScenarios {
  import Grammar.paragraph
  
  parsing("The paragraph.") as paragraph should produce (
    Paragraph(SourceRange.Unknown, Seq(
      Text(SourceRange.Unknown, "The"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "paragraph.")
    ))
  )

  parsing("""
  |Line1
  |Line2
  """) as paragraph should produce (
    Paragraph(SourceRange.Unknown, Seq(
      Text(SourceRange.Unknown, "Line1"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "Line2")
    ))
  )
}
