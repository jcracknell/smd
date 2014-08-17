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
    TightOrderedList(Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingDot, Some(1), None), Seq(
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("1")
      )),
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("2")
      ))
    ))
  )

  parsing("""
  | 1) Item 1
  | 2) Item 2
  """) as orderedList should produce (
    TightOrderedList(Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingParenthesis, Some(1), None), Seq(
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("1")
      )),
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("2")
      ))
    ))
  )

  parsing("""
  | (1) Item 1
  | (2) Item 2
  """) as orderedList should produce (
    TightOrderedList(Counter(NumeralStyle.Arabic, SeparatorStyle.EnclosingParentheses, Some(1), None), Seq(
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("1")
      )),
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("2")
      ))
    ))
  )

  parsing("""
  | #. Item 1
  | #. Item 2
  """) as orderedList should produce (
    TightOrderedList(Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingDot, None, None), Seq(
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("1")
      )),
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("2")
      ))
    ))
  )

  parsing("""
  | 3. Item 3
  | 4. Item 4
  """) as orderedList should produce (
    TightOrderedList(Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingDot, Some(3), None), Seq(
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("3")
      )),
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("4")
      ))
    ))
  )

  parsing("""
  | iii. Item 3
  |  iv. Item 4
  """) as orderedList should produce(
    TightOrderedList(Counter(NumeralStyle.LowerRoman, SeparatorStyle.TrailingDot, Some(3), None), Seq(
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("3")
      )),
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("4")
      ))
    ))
  )

  parsing("""
  | III. Item 3
  |  IV. Item 4
  """) as orderedList should produce(
    TightOrderedList(Counter(NumeralStyle.UpperRoman, SeparatorStyle.TrailingDot, Some(3), None), Seq(
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("3")
      )),
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("4")
      ))
    ))
  )

  parsing("""
  | b. Item b
  | c. Item c
  """) as orderedList should produce (
    TightOrderedList(Counter(NumeralStyle.LowerAlpha, SeparatorStyle.TrailingDot, Some(2), None), Seq(
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("b")
      )),
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("c")
      ))
    ))
  )

  parsing("""
  | B. Item B
  | C. Item C
  """) as orderedList should produce (
    TightOrderedList(Counter(NumeralStyle.UpperAlpha, SeparatorStyle.TrailingDot, Some(2), None), Seq(
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("B")
      )),
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("C")
      ))
    ))
  )

  parsing("""
  | ex:1. Item 1
  | ex:2. Item 2
  """) as orderedList should produce (
    TightOrderedList(Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingDot, Some(1), Some("ex")), Seq(
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("1")
      )),
      TightOrderedList.Item(Seq(
        Text("Item"), Space(), Text("2")
      ))
    ))
  )

  parsing("""
  | (ex:#) good example
  |
  | (ex:#) bad example
  """) as orderedList should produce (
    LooseOrderedList(Counter(NumeralStyle.Arabic, SeparatorStyle.EnclosingParentheses, None, Some("ex")), Seq(
      LooseOrderedList.Item(Seq(
        Paragraph(Seq(
          Text("good"), Space(), Text("example")
        ))
      )),
      LooseOrderedList.Item(Seq(
        Paragraph(Seq(
          Text("bad"), Space(), Text("example")
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
    TightOrderedList(Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingDot, Some(1), None), Seq(
      TightOrderedList.Item(
        Seq(Text("a")),
        Seq(
          TightOrderedList(Counter(NumeralStyle.LowerAlpha, SeparatorStyle.TrailingParenthesis, Some(1), None), Seq(
            TightOrderedList.Item(
              Seq(Text("b")),
              Seq()
            ),
            TightOrderedList.Item(
              Seq(Text("c")),
              Seq()
            )
          ))
        )
      ),
      TightOrderedList.Item(
        Seq(Text("d")),
        Seq(
          TightUnorderedList(Seq(
            TightUnorderedList.Item(
              Seq(Text("e")),
              Seq()
            )
          ))
        )
      ),
      TightOrderedList.Item(
        Seq(Text("f")),
        Seq()
      )
    ))
  )
}
