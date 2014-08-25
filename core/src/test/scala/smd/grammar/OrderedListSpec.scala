package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class OrderedListSpec extends ParsingScenarios {
  import Grammar.orderedList
  import OrderedList._

  parsing("""
  | 1.text
  """) as orderedList should reject

  parsing("""
  | 1. Item 1
  | 2. Item 2
  """) as orderedList should produce (
    TightOrderedList(SourceRange.Unknown, Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingDot, Some(1), None), Seq(
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "1")
      )),
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "2")
      ))
    ))
  )

  parsing("""
  | 1) Item 1
  | 2) Item 2
  """) as orderedList should produce (
    TightOrderedList(SourceRange.Unknown, Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingParenthesis, Some(1), None), Seq(
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "1")
      )),
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "2")
      ))
    ))
  )

  parsing("""
  | (1) Item 1
  | (2) Item 2
  """) as orderedList should produce (
    TightOrderedList(SourceRange.Unknown, Counter(NumeralStyle.Arabic, SeparatorStyle.EnclosingParentheses, Some(1), None), Seq(
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "1")
      )),
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "2")
      ))
    ))
  )

  parsing("""
  | #. Item 1
  | #. Item 2
  """) as orderedList should produce (
    TightOrderedList(SourceRange.Unknown, Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingDot, None, None), Seq(
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "1")
      )),
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "2")
      ))
    ))
  )

  parsing("""
  | 3. Item 3
  | 4. Item 4
  """) as orderedList should produce (
    TightOrderedList(SourceRange.Unknown, Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingDot, Some(3), None), Seq(
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "3")
      )),
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "4")
      ))
    ))
  )

  parsing("""
  | iii. Item 3
  |  iv. Item 4
  """) as orderedList should produce(
    TightOrderedList(SourceRange.Unknown, Counter(NumeralStyle.LowerRoman, SeparatorStyle.TrailingDot, Some(3), None), Seq(
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "3")
      )),
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "4")
      ))
    ))
  )

  parsing("""
  | III. Item 3
  |  IV. Item 4
  """) as orderedList should produce(
    TightOrderedList(SourceRange.Unknown, Counter(NumeralStyle.UpperRoman, SeparatorStyle.TrailingDot, Some(3), None), Seq(
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "3")
      )),
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "4")
      ))
    ))
  )

  parsing("""
  | b. Item b
  | c. Item c
  """) as orderedList should produce (
    TightOrderedList(SourceRange.Unknown, Counter(NumeralStyle.LowerAlpha, SeparatorStyle.TrailingDot, Some(2), None), Seq(
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "b")
      )),
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "c")
      ))
    ))
  )

  parsing("""
  | B. Item B
  | C. Item C
  """) as orderedList should produce (
    TightOrderedList(SourceRange.Unknown, Counter(NumeralStyle.UpperAlpha, SeparatorStyle.TrailingDot, Some(2), None), Seq(
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "B")
      )),
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "C")
      ))
    ))
  )

  parsing("""
  | ex:1. Item 1
  | ex:2. Item 2
  """) as orderedList should produce (
    TightOrderedList(SourceRange.Unknown, Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingDot, Some(1), Some("ex")), Seq(
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "1")
      )),
      TightOrderedList.Item(SourceRange.Unknown, Seq(
        Text(SourceRange.Unknown, "Item"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "2")
      ))
    ))
  )

  parsing("""
  | (ex:#) good example
  |
  | (ex:#) bad example
  """) as orderedList should produce (
    LooseOrderedList(SourceRange.Unknown, Counter(NumeralStyle.Arabic, SeparatorStyle.EnclosingParentheses, None, Some("ex")), Seq(
      LooseOrderedList.Item(SourceRange.Unknown, Seq(
        Paragraph(SourceRange.Unknown, Seq(
          Text(SourceRange.Unknown, "good"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "example")
        ))
      )),
      LooseOrderedList.Item(SourceRange.Unknown, Seq(
        Paragraph(SourceRange.Unknown, Seq(
          Text(SourceRange.Unknown, "bad"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "example")
        ))
      ))
    ))
  )

  parsing("""
  | 1. a
  |     a) b
  |     b) c
  | 2. d
  |      * e
  | 3. f
  """) as orderedList should produce (
    TightOrderedList(SourceRange.Unknown, Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingDot, Some(1), None), Seq(
      TightOrderedList.Item(SourceRange.Unknown, 
        Seq(Text(SourceRange.Unknown, "a")),
        Seq(
          TightOrderedList(SourceRange.Unknown, Counter(NumeralStyle.LowerAlpha, SeparatorStyle.TrailingParenthesis, Some(1), None), Seq(
            TightOrderedList.Item(SourceRange.Unknown, 
              Seq(Text(SourceRange.Unknown, "b")),
              Seq()
            ),
            TightOrderedList.Item(SourceRange.Unknown, 
              Seq(Text(SourceRange.Unknown, "c")),
              Seq()
            )
          ))
        )
      ),
      TightOrderedList.Item(SourceRange.Unknown, 
        Seq(Text(SourceRange.Unknown, "d")),
        Seq(
          TightUnorderedList(SourceRange.Unknown, Seq(
            TightUnorderedList.Item(SourceRange.Unknown, 
              Seq(Text(SourceRange.Unknown, "e")),
              Seq()
            )
          ))
        )
      ),
      TightOrderedList.Item(SourceRange.Unknown, 
        Seq(Text(SourceRange.Unknown, "f")),
        Seq()
      )
    ))
  )
}
