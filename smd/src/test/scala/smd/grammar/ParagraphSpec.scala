package smd
package grammar

import smd.markdown._

class ParagraphSpec extends ProductionSpec {
  import Grammar.paragraph
  
  parsing("The paragraph.") as paragraph should produce (
    Paragraph(Seq(
      Text("The"), Space(), Text("paragraph.")
    ))
  )

  parsing("""
  |Line1
  |Line2
  """.trim.stripMargin) as paragraph should produce (
    Paragraph(Seq(
      Text("Line1"), Space(), Text("Line2")
    ))
  )
}
