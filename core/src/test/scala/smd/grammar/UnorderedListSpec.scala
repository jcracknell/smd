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
    TightUnorderedList(SourceRange.Unknown, Seq(
      TightUnorderedList.Item(SourceRange.Unknown, 
        Seq(Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "1")),
        Seq()
      ),
      TightUnorderedList.Item(SourceRange.Unknown, 
        Seq(Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "2")),
        Seq()
      )
    ))
  )

  parsing("""
  | • Item 1
  | • Item 2
  """) as unorderedList should produce (
    TightUnorderedList(SourceRange.Unknown, Seq(
      TightUnorderedList.Item(SourceRange.Unknown, 
        Seq(Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "1")),
        Seq()
      ),
      TightUnorderedList.Item(SourceRange.Unknown, 
        Seq(Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "2")),
        Seq()
      )
    ))
  )

  parsing("""
  | * Item 1
  | + Item 2
  """) as unorderedList should produce (
    TightUnorderedList(SourceRange.Unknown, Seq(
      TightUnorderedList.Item(SourceRange.Unknown,
        Seq(Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "1")),
        Seq()
      )
    ))
  )

  parsing("""
  |* Item 1
  |continues
  | * Item 2
  """) as unorderedList should produce (
    TightUnorderedList(SourceRange.Unknown, Seq(
      TightUnorderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "1"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "continues")
      )),
      TightUnorderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "2")
      ))
    ))
  )


  parsing("""
  | * Item 1
  |
  | * Item 2
  """) as unorderedList should produce (
    LooseUnorderedList(SourceRange.Unknown, Seq(
      LooseUnorderedList.Item(SourceRange.Unknown, Seq(
        Paragraph(SourceRange.Unknown, Seq(
          Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "1")
        ))
      )),
      LooseUnorderedList.Item(SourceRange.Unknown, Seq(
        Paragraph(SourceRange.Unknown, Seq(
          Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "2")
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
    LooseUnorderedList(SourceRange.Unknown, Seq(
      LooseUnorderedList.Item(SourceRange.Unknown, Seq(
        Paragraph(SourceRange.Unknown, Seq(
          Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "1")
        ))
      )),
      LooseUnorderedList.Item(SourceRange.Unknown, Seq(
        Paragraph(SourceRange.Unknown, Seq(
          Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "2")
        )),
        Paragraph(SourceRange.Unknown, Seq(
          Text(SourceRange.Unknown, "continues")
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
    TightUnorderedList(SourceRange.Unknown, Seq(
      TightUnorderedList.Item(SourceRange.Unknown, 
        Seq(Text(SourceRange.Unknown, "a")),
        Seq(
          TightUnorderedList(SourceRange.Unknown, Seq(
            TightUnorderedList.Item(SourceRange.Unknown, 
              Seq(Text(SourceRange.Unknown, "b")),
              Seq(
                TightUnorderedList(SourceRange.Unknown, Seq(
                  TightUnorderedList.Item(SourceRange.Unknown, 
                    Seq(Text(SourceRange.Unknown, "c")),
                    Seq()
                  )
                ))
              )
            ),
            TightUnorderedList.Item(SourceRange.Unknown, 
              Seq(Text(SourceRange.Unknown, "d")),
              Seq(
                TightUnorderedList(SourceRange.Unknown, Seq(
                  TightUnorderedList.Item(SourceRange.Unknown, 
                    Seq(Text(SourceRange.Unknown, "e")),
                    Seq()
                  ),
                  TightUnorderedList.Item(SourceRange.Unknown, 
                    Seq(Text(SourceRange.Unknown, "f")),
                    Seq()
                  )
                ))
              )
            )
          ))
        )
      ),
      TightUnorderedList.Item(SourceRange.Unknown, 
        Seq(Text(SourceRange.Unknown, "g")),
        Seq()
      )
    ))
  )
}
