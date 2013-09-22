package smd
package grammar

import smd.markdown._

class BlockquoteSpec extends ProductionSpec {
  def subject = Grammar.blockquote

  shouldParse(
    """
    |> Blockquote
    """.trim.stripMargin
  ) as (
    Blockquote(Seq(
      Paragraph(Seq(
        Text("Blockquote")
      ))
    ))
  )

  shouldParse(
    """
    |> Blockquote
    |> line 2
    """.trim.stripMargin
  ) as (
    Blockquote(Seq(
      Paragraph(Seq(
        Text("Blockquote"), Space(), Text("line"), Space(), Text("2")
      ))
    ))
  )

  shouldParse(
    """
    |> Blockquote
    |line 2
    """.trim.stripMargin
  ) as (
    Blockquote(Seq(
      Paragraph(Seq(
        Text("Blockquote"), Space(), Text("line"), Space(), Text("2")
      ))
    ))
  )

  shouldParse(
    """
    |> Blockquote
    |
    |> continues
    """.trim.stripMargin
  ) as (
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
