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
    Table(SourceRange.Unknown, 
      head = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "head1"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "head2")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "body1"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "body2")))
        )
      )
    )
  )
  
  parsing("""
  ||head1|head2
  ||-----|-----
  ||body1|body2
  """) as table should produce (
    Table(SourceRange.Unknown, 
      head = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "head1"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "head2")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "body1"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "body2")))
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
    Table(SourceRange.Unknown, 
      head = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "head1")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "body1")))
        )
      )
    )
  )

  parsing("""
  ||head1|
  ||-----|
  ||body1|
  """) as table should produce (
    Table(SourceRange.Unknown, 
      head = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "head1")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "body1")))
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
    Table(SourceRange.Unknown, 
      head = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "head1"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "head2")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "body1"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "body2")))
        ),
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq()),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq())
        ),
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq())
        )
      )
    )
  )

  parsing("""
  || Left  | Center |  Right |
  ||:------+:------:+-------:|
  || left  | center |  right |
  """) as table should produce (
    Table(SourceRange.Unknown, 
      head = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left,   1, Seq(Text(SourceRange.Unknown, "Left"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Center, 1, Seq(Text(SourceRange.Unknown, "Center"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right,  1, Seq(Text(SourceRange.Unknown, "Right")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left,   1, Seq(Text(SourceRange.Unknown, "left"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Center, 1, Seq(Text(SourceRange.Unknown, "center"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right,  1, Seq(Text(SourceRange.Unknown, "right")))
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
    Table(SourceRange.Unknown, 
      head = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "head1"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "head2")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "body1"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "body2")))
        ),
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "body3"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left, 1, Seq(Text(SourceRange.Unknown, "body4")))
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
    Table(SourceRange.Unknown, 
      head = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 1, Seq(Text(SourceRange.Unknown, "Trial"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 2, Seq(Text(SourceRange.Unknown, "1"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 2, Seq(Text(SourceRange.Unknown, "2")))
        ),
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 1, Seq(Text(SourceRange.Unknown, "Component"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 1, Seq(Text(SourceRange.Unknown, "Min"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 1, Seq(Text(SourceRange.Unknown, "Max"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 1, Seq(Text(SourceRange.Unknown, "Min"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 1, Seq(Text(SourceRange.Unknown, "Max")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 1, Seq(Text(SourceRange.Unknown, "Frobnicator"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 1, Seq(Text(SourceRange.Unknown, "23"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 1, Seq(Text(SourceRange.Unknown, "42"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 1, Seq(Text(SourceRange.Unknown, "20"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 1, Seq(Text(SourceRange.Unknown, "56")))
        ),
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 1, Seq(Text(SourceRange.Unknown, "Confoobmotron"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 1, Seq(Text(SourceRange.Unknown, "57"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 1, Seq(Text(SourceRange.Unknown, "130"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 1, Seq(Text(SourceRange.Unknown, "63"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right, 1, Seq(Text(SourceRange.Unknown, "112")))
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
    Table(SourceRange.Unknown, 
      head = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left,   1, Seq(Text(SourceRange.Unknown, "Left"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Center, 1, Seq(Text(SourceRange.Unknown, "Center"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right,  1, Seq(Text(SourceRange.Unknown, "Right")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left,   1, Seq(Text(SourceRange.Unknown, "span1"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Center, 1, Seq(Text(SourceRange.Unknown, "span1"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right,  1, Seq(Text(SourceRange.Unknown, "span1")))
        ),
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left,   2, Seq(Text(SourceRange.Unknown, "span2"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right,  1, Seq(Text(SourceRange.Unknown, "span1")))
        ),
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left,   1, Seq(Text(SourceRange.Unknown, "span1"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Center, 2, Seq(Text(SourceRange.Unknown, "span2")))
        ),
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Left,   3, Seq(Text(SourceRange.Unknown, "span3")))
        )
      )
    )
  )

  parsing("""
  || head1 | head2 | head3 |
  ||-------------:|+:-----:|
  || body1 | body2 | body3 |
  """) as table should produce (
    Table(SourceRange.Unknown, 
      head = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right,  1, Seq(Text(SourceRange.Unknown, "head1"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right,  1, Seq(Text(SourceRange.Unknown, "head2"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Center, 1, Seq(Text(SourceRange.Unknown, "head3")))
        )
      ),
      body = Seq(
        Table.Row(
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right,  1, Seq(Text(SourceRange.Unknown, "body1"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Right,  1, Seq(Text(SourceRange.Unknown, "body2"))),
          Table.Cell(SourceRange.Unknown, Table.CellAlignment.Center, 1, Seq(Text(SourceRange.Unknown, "body3")))
        )
      )
    )
  )
}
