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
    TightUnorderedList(Seq(
      TightUnorderedList.Item(
        Seq(Text("Item"), Space(), Text("1")),
        Seq()
      ),
      TightUnorderedList.Item(
        Seq(Text("Item"), Space(), Text("2")),
        Seq()
      )
    ))
  )

  parsing("""
  |* Item 1
  |continues
  | * Item 2
  """) as unorderedList should produce (
    TightUnorderedList(Seq(
      TightUnorderedList.Item(Seq(
        Text("Item"), Space(), Text("1"), Space(), Text("continues")
      )),
      TightUnorderedList.Item(Seq(
        Text("Item"), Space(), Text("2")
      ))
    ))
  )


  parsing("""
  | * Item 1
  |
  | * Item 2
  """) as unorderedList should produce (
    LooseUnorderedList(Seq(
      LooseUnorderedList.Item(Seq(
        Paragraph(Seq(
          Text("Item"), Space(), Text("1")
        ))
      )),
      LooseUnorderedList.Item(Seq(
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
    LooseUnorderedList(Seq(
      LooseUnorderedList.Item(Seq(
        Paragraph(Seq(
          Text("Item"), Space(), Text("1")
        ))
      )),
      LooseUnorderedList.Item(Seq(
        Paragraph(Seq(
          Text("Item"), Space(), Text("2")
        )),
        Paragraph(Seq(
          Text("continues")
        ))
      ))
    ))
  )

  parsing("""
  |  * a
  |      * b
  |          * c
  |      * d
  |          * e
  |          * f
  |  * g
  """) as unorderedList should produce (
    TightUnorderedList(Seq(
      TightUnorderedList.Item(
        Seq(Text("a")),
        Seq(
          TightUnorderedList(Seq(
            TightUnorderedList.Item(
              Seq(Text("b")),
              Seq(
                TightUnorderedList(Seq(
                  TightUnorderedList.Item(
                    Seq(Text("c")),
                    Seq()
                  )
                ))
              )
            ),
            TightUnorderedList.Item(
              Seq(Text("d")),
              Seq(
                TightUnorderedList(Seq(
                  TightUnorderedList.Item(
                    Seq(Text("e")),
                    Seq()
                  ),
                  TightUnorderedList.Item(
                    Seq(Text("f")),
                    Seq()
                  )
                ))
              )
            )
          ))
        )
      ),
      TightUnorderedList.Item(
        Seq(Text("g")),
        Seq()
      )
    ))
  )
}
