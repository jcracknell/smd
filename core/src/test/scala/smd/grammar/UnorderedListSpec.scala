package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class UnorderedListSpec extends ParsingScenarios {
  import Grammar.unorderedList

  parsing("""
  | * Item 1
  | * Item 2
  """) as unorderedList should produce (
    UnorderedList.Tight(Seq(
      UnorderedList.Item(Seq(
        Text("Item"), Space(), Text("1")
      )),
      UnorderedList.Item(Seq(
        Text("Item"), Space(), Text("2")
      ))
    ))
  )

  parsing( """
  |* Item 1
  |continues
  | * Item 2
  """) as unorderedList should produce (
    UnorderedList.Tight(Seq(
      UnorderedList.Item(Seq(
        Text("Item"), Space(), Text("1"), Space(), Text("continues")
      )),
      UnorderedList.Item(Seq(
        Text("Item"), Space(), Text("2")
      ))
    ))
  )


  parsing("""
  | * Item 1
  |
  | * Item 2
  """) as unorderedList should produce (
    UnorderedList.Loose(Seq(
      UnorderedList.Item(Seq(
        Paragraph(Seq(
          Text("Item"), Space(), Text("1")
        ))
      )),
      UnorderedList.Item(Seq(
        Paragraph(Seq(
          Text("Item"), Space(), Text("2")
        ))
      ))
    ))
  )

  parsing("""
  |  * Item 1
  |  * Item 2
  |
  |    continues
  """) as unorderedList should produce (
    UnorderedList.Loose(Seq(
      UnorderedList.Item(Seq(
        Paragraph(Seq(
          Text("Item"), Space(), Text("1")
        ))
      )),
      UnorderedList.Item(Seq(
        Paragraph(Seq(
          Text("Item"), Space(), Text("2")
        )),
        Paragraph(Seq(
          Text("continues")
        ))
      ))
    ))
  )
}
