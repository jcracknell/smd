package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class HeadingSpec extends ParsingScenarios {
  import Grammar.heading

  parsing("""
  |# Heading 1
  """) as heading should produce (
    Heading(1, Seq(
      Text("Heading"), Space(), Text("1")
    ))
  )

  parsing("""
  |## Heading 2
  """) as heading should produce (
    Heading(2, Seq(
      Text("Heading"), Space(), Text("2")
    ))
  )
}
