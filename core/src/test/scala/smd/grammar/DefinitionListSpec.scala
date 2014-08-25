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
    TightDefinitionList(SourceRange.Unknown, Seq(
      TightDefinitionList.Item(SourceRange.Unknown, 
        DefinitionList.Term(SourceRange.Unknown, Seq(
          Text(SourceRange.Unknown, "term")
        )),
        Seq(
          TightDefinitionList.Definition(SourceRange.Unknown, Seq(
            Text(SourceRange.Unknown, "definition")
          ))
        )
      )
    ))
  )

  parsing("""
  |term
  |  : multi
  |    line
  """) as definitionList should produce (
    TightDefinitionList(SourceRange.Unknown, Seq(
      TightDefinitionList.Item(SourceRange.Unknown, 
        DefinitionList.Term(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "term"))),
        Seq(
          TightDefinitionList.Definition(SourceRange.Unknown, Seq(
            Text(SourceRange.Unknown, "multi"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "line")
          ))
        )
      )
    ))
  )

  parsing("""
  |term
  |  : multi
  |   line
  """) as definitionList should produce (
    TightDefinitionList(SourceRange.Unknown, Seq(
      TightDefinitionList.Item(SourceRange.Unknown, 
        DefinitionList.Term(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "term"))),
        Seq(
          TightDefinitionList.Definition(SourceRange.Unknown, Seq(
            Text(SourceRange.Unknown, "multi"), Space(SourceRange.Unknown), Text(SourceRange.Unknown, "line")
          ))
        )
      )
    ))
  )

  parsing("""
  |term
  |~ definition
  """) as definitionList should produce (
    TightDefinitionList(SourceRange.Unknown, Seq(
      TightDefinitionList.Item(SourceRange.Unknown, 
        DefinitionList.Term(SourceRange.Unknown, Seq(
          Text(SourceRange.Unknown, "term")
        )),
        Seq(
          TightDefinitionList.Definition(SourceRange.Unknown, Seq(
            Text(SourceRange.Unknown, "definition")
          ))
        )
      )
    ))
  )

  // TODO: This test is commented out because the behavior of definition lists is... tricky.
  // How do you determine if `term2` below is a continuation of `def2`?
  /*
  parsing("""
  |term1
  |: def1
  |: def2
  |term2
  |: def1
  |: def2
  """) as definitionList should produce (
    TightDefinitionList(SourceRange.Unknown, Seq(
      TightDefinitionList.Item(SourceRange.Unknown, 
        DefinitionList.Term(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "term1"))),
        Seq(
          TightDefinitionList.Definition(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "def1"))),
          TightDefinitionList.Definition(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "def2")))
        )
      ),
      TightDefinitionList.Item(SourceRange.Unknown, 
        DefinitionList.Term(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "term2"))),
        Seq(
          TightDefinitionList.Definition(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "def1"))),
          TightDefinitionList.Definition(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "def2")))
        )
      )
    ))
  )
  */

  parsing("""
  |term1
  |: def1
  |: def2
  |
  |term2
  |: def1
  |: def2
  """) as definitionList should produce (
    LooseDefinitionList(SourceRange.Unknown, Seq(
      LooseDefinitionList.Item(SourceRange.Unknown, 
        DefinitionList.Term(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "term1"))),
        Seq(
          LooseDefinitionList.Definition(SourceRange.Unknown, Seq(
            Paragraph(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "def1")))
          )),
          LooseDefinitionList.Definition(SourceRange.Unknown, Seq(
            Paragraph(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "def2")))
          ))
        )
      ),
      LooseDefinitionList.Item(SourceRange.Unknown, 
        DefinitionList.Term(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "term2"))),
        Seq(
          LooseDefinitionList.Definition(SourceRange.Unknown, Seq(
            Paragraph(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "def1")))
          )),
          LooseDefinitionList.Definition(SourceRange.Unknown, Seq(
            Paragraph(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "def2")))
          ))
        )
      )
    ))
  )

  parsing("""
  |term
  |
  |: def1
  |: def2
  """) as definitionList should produce (
    LooseDefinitionList(SourceRange.Unknown, Seq(
      LooseDefinitionList.Item(SourceRange.Unknown, 
        DefinitionList.Term(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "term"))),
        Seq(
          LooseDefinitionList.Definition(SourceRange.Unknown, Seq(
            Paragraph(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "def1")))
          )),
          LooseDefinitionList.Definition(SourceRange.Unknown, Seq(
            Paragraph(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "def2")))
          ))
        )
      )
    ))
  )

  parsing("""
  |term
  |: def1
  |
  |: def2
  """) as definitionList should produce (
    LooseDefinitionList(SourceRange.Unknown, Seq(
      LooseDefinitionList.Item(SourceRange.Unknown, 
        DefinitionList.Term(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "term"))),
        Seq(
          LooseDefinitionList.Definition(SourceRange.Unknown, Seq(
            Paragraph(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "def1")))
          )),
          LooseDefinitionList.Definition(SourceRange.Unknown, Seq(
            Paragraph(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "def2")))
          ))
        )
      )
    ))
  )

  /*
  parsing("""
  |a
  |  : b
  |      * c
  |      * d
  |  : e
  |f
  |  ~ g
  """) as definitionList should produce (
    TightDefinitionList(SourceRange.Unknown, Seq(
      TightDefinitionList.Item(SourceRange.Unknown, 
        DefinitionList.Term(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "a"))),
        Seq(
          TightDefinitionList.Definition(SourceRange.Unknown, 
            Seq(Text(SourceRange.Unknown, "b")),
            Seq(
              TightUnorderedList(SourceRange.Unknown, Seq(
                TightUnorderedList.Item(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "c"))),
                TightUnorderedList.Item(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "d")))
              ))
            )
          ),
          TightDefinitionList.Definition(SourceRange.Unknown, 
            Seq(Text(SourceRange.Unknown, "e"))
          )
        )
      ),
      TightDefinitionList.Item(SourceRange.Unknown, 
        DefinitionList.Term(SourceRange.Unknown, Seq(Text(SourceRange.Unknown, "f"))),
        Seq(
          TightDefinitionList.Definition(SourceRange.Unknown, 
            Seq(Text(SourceRange.Unknown, "g"))
          )
        )
      )
    ))
  )
  */
}
