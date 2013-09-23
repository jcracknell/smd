package smd
package grammar

import smd.markdown._

class UnorderedListSpec extends ProductionSpec {
  def subject = Grammar.unorderedList

  shouldParse(
    """ * Item 1
      | * Item 2
    """.trim.stripMargin
  ) as UnorderedList.Tight(Seq(
    UnorderedList.Item(Seq(
      Text("Item"), Space(), Text("1")
    )),
    UnorderedList.Item(Seq(
      Text("Item"), Space(), Text("2")
    ))
  ))

  shouldParse(
    """ * Item 1
      |continues
      | * Item 2
    """.trim.stripMargin
  ) as UnorderedList.Tight(Seq(
    UnorderedList.Item(Seq(
      Text("Item"), Space(), Text("1"), Space(), Text("continues")
    )),
    UnorderedList.Item(Seq(
      Text("Item"), Space(), Text("2")
    ))
  ))


  shouldParse(
    """ * Item 1
      |
      | * Item 2
    """.trim.stripMargin
  ) as UnorderedList.Loose(Seq(
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

  shouldParse(
    """  * Item 1
      |  * Item 2
      |
      |    continues
    """.trim.stripMargin
  ) as UnorderedList.Loose(Seq(
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
}
