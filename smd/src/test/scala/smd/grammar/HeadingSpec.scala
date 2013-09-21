package smd
package grammar

import smd.markdown._

class HeadingSpec extends ProductionSpec {
  def subject = Grammar.heading

  shouldParse(
    """
    |# Heading 1
    """.trim.stripMargin
  ) as (
    Heading(1, Seq(
      Text("Heading"), Space(), Text("1")
    ))
  )

  shouldParse(
    """
    |## Heading 2
    """.trim.stripMargin
  ) as (
    Heading(2, Seq(
      Text("Heading"), Space(), Text("2")
    ))
  )
}
