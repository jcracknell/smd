package smd
package grammar

import smd.markdown._

class ParagraphSpec extends ProductionSpec {
  def subject = Grammar.paragraph

  shouldParse("The paragraph.") as (
    Paragraph(Seq(
      Text("The"), Space(), Text("paragraph.")
    ))
  )
  shouldParse("""
  |Line1
  |Line2
  """.trim.stripMargin) as (
    Paragraph(Seq(
      Text("Line1"), Space(), Text("Line2")
    ))
  )
}
