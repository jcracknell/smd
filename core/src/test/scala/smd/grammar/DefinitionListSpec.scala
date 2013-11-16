package smd
package grammar

import smd.dom._
import smd.parsing.ParsingScenarios

class DefinitionListSpec extends ParsingScenarios {
  import Grammar.definitionList

  parsing("""
  |term
  |: definition
  """) as definitionList should produce (
    DefinitionList.Tight(Seq(
      DefinitionList.Item(
        DefinitionList.Term(Seq(
          Text("term")
        )),
        DefinitionList.Definition(Seq(
          Text("definition")
        ))
      )
    ))
  )

  parsing("""
  |term
  |  : multi
  |    line
  """) as definitionList should produce (
    DefinitionList.Tight(Seq(
      DefinitionList.Item(
        DefinitionList.Term(Seq(Text("term"))),
        DefinitionList.Definition(Seq(
          Text("multi"), Space(), Text("line")
        ))
      )
    ))
  )

  parsing("""
  |term
  |  : multi
  |   line
  """) as definitionList should produce (
    DefinitionList.Tight(Seq(
      DefinitionList.Item(
        DefinitionList.Term(Seq(Text("term"))),
        DefinitionList.Definition(Seq(Text("multi")))
      )
    ))
  )

  parsing("""
  |term
  |~ definition
  """) as definitionList should produce (
    DefinitionList.Tight(Seq(
      DefinitionList.Item(
        DefinitionList.Term(Seq(
          Text("term")
        )),
        DefinitionList.Definition(Seq(
          Text("definition")
        ))
      )
    ))
  )

  parsing("""
  |term1
  |: def1
  |: def2
  |term2
  |: def1
  |: def2
  """) as definitionList should produce (
    DefinitionList.Tight(Seq(
      DefinitionList.Item(
        DefinitionList.Term(Seq(Text("term1"))),
        DefinitionList.Definition(Seq(Text("def1"))),
        DefinitionList.Definition(Seq(Text("def2")))
      ),
      DefinitionList.Item(
        DefinitionList.Term(Seq(Text("term2"))),
        DefinitionList.Definition(Seq(Text("def1"))),
        DefinitionList.Definition(Seq(Text("def2")))
      )
    ))
  )

  parsing("""
  |term1
  |: def1
  |: def2
  |
  |term2
  |: def1
  |: def2
  """) as definitionList should produce (
    DefinitionList.Loose(Seq(
      DefinitionList.Item(
        DefinitionList.Term(Seq(Text("term1"))),
        DefinitionList.Definition(Seq(
          Paragraph(Seq(Text("def1")))
        )),
        DefinitionList.Definition(Seq(
          Paragraph(Seq(Text("def2")))
        ))
      ),
      DefinitionList.Item(
        DefinitionList.Term(Seq(Text("term2"))),
        DefinitionList.Definition(Seq(
          Paragraph(Seq(Text("def1")))
        )),
        DefinitionList.Definition(Seq(
          Paragraph(Seq(Text("def2")))
        ))
      )
    ))
  )

  parsing("""
  |term
  |
  |: def1
  |: def2
  """) as definitionList should produce (
    DefinitionList.Loose(Seq(
      DefinitionList.Item(
        DefinitionList.Term(Seq(Text("term"))),
        DefinitionList.Definition(Seq(
          Paragraph(Seq(Text("def1")))
        )),
        DefinitionList.Definition(Seq(
          Paragraph(Seq(Text("def2")))
        ))
      )
    ))
  )

  parsing("""
  |term
  |: def1
  |
  |: def2
  """) as definitionList should produce (
    DefinitionList.Loose(Seq(
      DefinitionList.Item(
        DefinitionList.Term(Seq(Text("term"))),
        DefinitionList.Definition(Seq(
          Paragraph(Seq(Text("def1")))
        )),
        DefinitionList.Definition(Seq(
          Paragraph(Seq(Text("def2")))
        ))
      )
    ))
  )
}
