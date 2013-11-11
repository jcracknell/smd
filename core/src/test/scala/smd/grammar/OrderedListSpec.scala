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
    OrderedList.Tight(Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingDot, Some(1), None), Seq(
      OrderedList.Item(None, Seq(
        Text("Item"), Space(), Text("1")
      )),
      OrderedList.Item(None, Seq(
        Text("Item"), Space(), Text("2")
      ))
    ))
  )

  parsing("""
  | 1) Item 1
  | 2) Item 2
  """) as orderedList should produce (
    OrderedList.Tight(Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingParenthesis, Some(1), None), Seq(
      OrderedList.Item(None, Seq(
        Text("Item"), Space(), Text("1")
      )),
      OrderedList.Item(None, Seq(
        Text("Item"), Space(), Text("2")
      ))
    ))
  )

  parsing("""
  | (1) Item 1
  | (2) Item 2
  """) as orderedList should produce (
    OrderedList.Tight(Counter(NumeralStyle.Arabic, SeparatorStyle.EnclosingParentheses, Some(1), None), Seq(
      OrderedList.Item(None, Seq(
        Text("Item"), Space(), Text("1")
      )),
      OrderedList.Item(None, Seq(
        Text("Item"), Space(), Text("2")
      ))
    ))
  )

  parsing("""
  | #. Item 1
  | #. Item 2
  """) as orderedList should produce (
    OrderedList.Tight(Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingDot, None, None), Seq(
      OrderedList.Item(None, Seq(
        Text("Item"), Space(), Text("1")
      )),
      OrderedList.Item(None, Seq(
        Text("Item"), Space(), Text("2")
      ))
    ))
  )

  parsing("""
  | 3. Item 3
  | 4. Item 4
  """) as orderedList should produce (
    OrderedList.Tight(Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingDot, Some(3), None), Seq(
      OrderedList.Item(None, Seq(
        Text("Item"), Space(), Text("3")
      )),
      OrderedList.Item(None, Seq(
        Text("Item"), Space(), Text("4")
      ))
    ))
  )

  parsing("""
  | b. Item b
  | c. Item c
  """) as orderedList should produce (
    OrderedList.Tight(Counter(NumeralStyle.LowerAlpha, SeparatorStyle.TrailingDot, Some(2), None), Seq(
      OrderedList.Item(None, Seq(
        Text("Item"), Space(), Text("b")
      )),
      OrderedList.Item(None, Seq(
        Text("Item"), Space(), Text("c")
      ))
    ))
  )

  parsing("""
  | B. Item B
  | C. Item C
  """) as orderedList should produce (
    OrderedList.Tight(Counter(NumeralStyle.UpperAlpha, SeparatorStyle.TrailingDot, Some(2), None), Seq(
      OrderedList.Item(None, Seq(
        Text("Item"), Space(), Text("B")
      )),
      OrderedList.Item(None, Seq(
        Text("Item"), Space(), Text("C")
      ))
    ))
  )

  parsing("""
  | ex:1. Item 1
  | ex:2. Item 2
  """) as orderedList should produce (
    OrderedList.Tight(Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingDot, Some(1), Some("ex")), Seq(
      OrderedList.Item(None, Seq(
        Text("Item"), Space(), Text("1")
      )),
      OrderedList.Item(None, Seq(
        Text("Item"), Space(), Text("2")
      ))
    ))
  )

  parsing("""
  | 1[first]. Item 1
  | 2[second]. Item 2
  """) as orderedList should produce (
    OrderedList.Tight(Counter(NumeralStyle.Arabic, SeparatorStyle.TrailingDot, Some(1), None), Seq(
      OrderedList.Item(Some(ReferenceId("first")), Seq(
        Text("Item"), Space(), Text("1")
      )),
      OrderedList.Item(Some(ReferenceId("second")), Seq(
        Text("Item"), Space(), Text("2")
      ))
    ))
  )

  parsing("""
  | (ex:#[good]) good example
  |
  | (ex:#[bad]) bad example
  """) as orderedList should produce (
    OrderedList.Loose(Counter(NumeralStyle.Arabic, SeparatorStyle.EnclosingParentheses, None, Some("ex")), Seq(
      OrderedList.Item(Some(ReferenceId("good")), Seq(
        Paragraph(Seq(
          Text("good"), Space(), Text("example")
        ))
      )),
      OrderedList.Item(Some(ReferenceId("bad")), Seq(
        Paragraph(Seq(
          Text("bad"), Space(), Text("example")
        ))
      ))
    ))
  )
}
