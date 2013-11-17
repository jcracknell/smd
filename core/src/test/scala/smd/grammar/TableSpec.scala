package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class TableSpec extends ParsingScenarios {
  import Grammar.table

  // Recall:
  // the leading pipe character is stripped from the input string as part of the margin

  parsing("""
  ||head1|head2|
  ||-----|-----|
  ||body1|body2|
  """) as table should produce (
    Table(
      head = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("head1"))),
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("head2")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("body1"))),
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("body2")))
        )
      )
    )
  )
  
  parsing("""
  ||head1|head2
  ||-----|-----
  ||body1|body2
  """) as table should produce (
    Table(
      head = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("head1"))),
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("head2")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("body1"))),
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("body2")))
        )
      )
    )
  )

  parsing("""
  |head1|head2|
  |-----|-----|
  |body1|body2|
  """) as table should reject

  parsing("""
  ||head1
  ||-----
  ||body1
  """) as table should produce (
    Table(
      head = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("head1")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("body1")))
        )
      )
    )
  )

  parsing("""
  ||head1|
  ||-----|
  ||body1|
  """) as table should produce (
    Table(
      head = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("head1")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("body1")))
        )
      )
    )
  )

  parsing("""
  ||head1|head2          |//comment
  ||-----|---------------| //comment
  ||body1|body2          | /*comment*/
  ||     | /* comment */ |
  || /**/| //comment
  """) as table should produce (
    Table(
      head = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("head1"))),
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("head2")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("body1"))),
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("body2")))
        ),
        Table.Row(
          Table.Cell(Table.CellAlignment.Left, 1, Seq()),
          Table.Cell(Table.CellAlignment.Left, 1, Seq())
        ),
        Table.Row(
          Table.Cell(Table.CellAlignment.Left, 1, Seq())
        )
      )
    )
  )

  parsing("""
  || Left  | Center |  Right |
  ||:------+:------:+-------:|
  || left  | center |  right |
  """) as table should produce (
    Table(
      head = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Left,   1, Seq(Text("Left"))),
          Table.Cell(Table.CellAlignment.Center, 1, Seq(Text("Center"))),
          Table.Cell(Table.CellAlignment.Right,  1, Seq(Text("Right")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Left,   1, Seq(Text("left"))),
          Table.Cell(Table.CellAlignment.Center, 1, Seq(Text("center"))),
          Table.Cell(Table.CellAlignment.Right,  1, Seq(Text("right")))
        )
      )
    )
  )

  parsing("""
  ||head1|head2|
  ||-----|-----|
  |// Trial 1
  ||body1|body2|
  |/* Trial 2 */
  ||body3|body4|
  """) as table should produce (
    Table(
      head = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("head1"))),
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("head2")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("body1"))),
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("body2")))
        ),
        Table.Row(
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("body3"))),
          Table.Cell(Table.CellAlignment.Left, 1, Seq(Text("body4")))
        )
      )
    )
  )
  
  parsing("""
  ||         Trial |     1    ||     2    ||
  ||     Component | Min | Max | Min | Max |
  ||--------------:+----:+----:+----:+----:|
  ||   Frobnicator |  23 |  42 |  20 |  56 |
  || Confoobmotron |  57 | 130 |  63 | 112 |
  """) as table should produce (
    Table(
      head = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Right, 1, Seq(Text("Trial"))),
          Table.Cell(Table.CellAlignment.Right, 2, Seq(Text("1"))),
          Table.Cell(Table.CellAlignment.Right, 2, Seq(Text("2")))
        ),
        Table.Row(
          Table.Cell(Table.CellAlignment.Right, 1, Seq(Text("Component"))),
          Table.Cell(Table.CellAlignment.Right, 1, Seq(Text("Min"))),
          Table.Cell(Table.CellAlignment.Right, 1, Seq(Text("Max"))),
          Table.Cell(Table.CellAlignment.Right, 1, Seq(Text("Min"))),
          Table.Cell(Table.CellAlignment.Right, 1, Seq(Text("Max")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Right, 1, Seq(Text("Frobnicator"))),
          Table.Cell(Table.CellAlignment.Right, 1, Seq(Text("23"))),
          Table.Cell(Table.CellAlignment.Right, 1, Seq(Text("42"))),
          Table.Cell(Table.CellAlignment.Right, 1, Seq(Text("20"))),
          Table.Cell(Table.CellAlignment.Right, 1, Seq(Text("56")))
        ),
        Table.Row(
          Table.Cell(Table.CellAlignment.Right, 1, Seq(Text("Confoobmotron"))),
          Table.Cell(Table.CellAlignment.Right, 1, Seq(Text("57"))),
          Table.Cell(Table.CellAlignment.Right, 1, Seq(Text("130"))),
          Table.Cell(Table.CellAlignment.Right, 1, Seq(Text("63"))),
          Table.Cell(Table.CellAlignment.Right, 1, Seq(Text("112")))
        )
      )
    )
  )

  parsing("""
  || Left   | Center |  Right |
  ||:-------+:------:+-------:|
  || span1  | span1  |  span1 |
  || span2          ||  span1 |
  || span1  | span2          ||
  || span3                  |||
  """) as table should produce (
    Table(
      head = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Left,   1, Seq(Text("Left"))),
          Table.Cell(Table.CellAlignment.Center, 1, Seq(Text("Center"))),
          Table.Cell(Table.CellAlignment.Right,  1, Seq(Text("Right")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Left,   1, Seq(Text("span1"))),
          Table.Cell(Table.CellAlignment.Center, 1, Seq(Text("span1"))),
          Table.Cell(Table.CellAlignment.Right,  1, Seq(Text("span1")))
        ),
        Table.Row(
          Table.Cell(Table.CellAlignment.Left,   2, Seq(Text("span2"))),
          Table.Cell(Table.CellAlignment.Right,  1, Seq(Text("span1")))
        ),
        Table.Row(
          Table.Cell(Table.CellAlignment.Left,   1, Seq(Text("span1"))),
          Table.Cell(Table.CellAlignment.Center, 2, Seq(Text("span2")))
        ),
        Table.Row(
          Table.Cell(Table.CellAlignment.Left,   3, Seq(Text("span3")))
        )
      )
    )
  )

  parsing("""
  || head1 | head2 | head3 |
  ||-------------:|+:-----:|
  || body1 | body2 | body3 |
  """) as table should produce (
    Table(
      head = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Right,  1, Seq(Text("head1"))),
          Table.Cell(Table.CellAlignment.Right,  1, Seq(Text("head2"))),
          Table.Cell(Table.CellAlignment.Center, 1, Seq(Text("head3")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(Table.CellAlignment.Right,  1, Seq(Text("body1"))),
          Table.Cell(Table.CellAlignment.Right,  1, Seq(Text("body2"))),
          Table.Cell(Table.CellAlignment.Center, 1, Seq(Text("body3")))
        )
      )
    )
  )
}
