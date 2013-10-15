package smd
package grammar

import smd.markdown._
import smd.parsing.ParsingScenarios

class BlockquoteSpec extends ParsingScenarios {
  import Grammar.blockquote

  parsing("""
    |> Blockquote
  """.trim.stripMargin) as blockquote should produce (
    Blockquote(Seq(
      Paragraph(Seq(
        Text("Blockquote")
      ))
    ))
  )

  parsing( """
    |> Blockquote
    |> line 2
  """.trim.stripMargin) as blockquote should produce (
    Blockquote(Seq(
      Paragraph(Seq(
        Text("Blockquote"), Space(), Text("line"), Space(), Text("2")
      ))
    ))
  )

  parsing(
    """
    |> Blockquote
    |line 2
    """.trim.stripMargin
  ) as blockquote should produce (
    Blockquote(Seq(
      Paragraph(Seq(
        Text("Blockquote"), Space(), Text("line"), Space(), Text("2")
      ))
    ))
  )

  parsing(
    """
    |> Blockquote
    |
    |> continues
    """.trim.stripMargin
  ) as blockquote should produce (
    Blockquote(Seq(
      Paragraph(Seq(
        Text("Blockquote")
      )),
      Paragraph(Seq(
        Text("continues")
      ))
    ))
  )
}
