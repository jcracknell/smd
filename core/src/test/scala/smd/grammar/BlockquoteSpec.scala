package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class BlockquoteSpec extends ParsingScenarios {
  import Grammar.blockquote

  parsing("""
  |> Blockquote
  """) as blockquote should produce (
    Blockquote(SourceRange.Unknown, Seq(
      Paragraph(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Blockquote")
      ))
    ))
  )

  parsing("""
  |> Blockquote
  |> line 2
  """) as blockquote should produce (
    Blockquote(SourceRange.Unknown, Seq(
      Paragraph(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Blockquote"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "line"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "2")
      ))
    ))
  )

  parsing("""
  |> Blockquote
  |line 2
  """) as blockquote should produce (
    Blockquote(SourceRange.Unknown, Seq(
      Paragraph(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Blockquote"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "line"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "2")
      ))
    ))
  )

  parsing("""
  |> Blockquote
  |
  |> ends
  """) as blockquote should produce (
    Blockquote(SourceRange.Unknown, Seq(
      Paragraph(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Blockquote")
      ))
    ))
  )

  parsing("""
  |> Blockquote
  |>
  |> continues
  """) as blockquote should produce (
    Blockquote(SourceRange.Unknown, Seq(
      Paragraph(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Blockquote")
      )),
      Paragraph(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "continues")
      ))
    ))
  )
}
