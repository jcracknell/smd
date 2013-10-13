package smd
package grammar

import smd.markdown._

class HeadingSpec extends ProductionSpec {
  import Grammar.heading

  parsing(
    """
    |# Heading 1
    """.trim.stripMargin
  ) as heading should produce (
    Heading(1, Seq(
      Text("Heading"), Space(), Text("1")
    ))
  )

  parsing(
    """
    |## Heading 2
    """.trim.stripMargin
  ) as heading should produce (
    Heading(2, Seq(
      Text("Heading"), Space(), Text("2")
    ))
  )
}
