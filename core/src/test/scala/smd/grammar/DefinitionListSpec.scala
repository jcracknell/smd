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
    TightDefinitionList(Seq(
      TightDefinitionList.Item(
        DefinitionList.Term(Seq(
          Text("term")
        )),
        Seq(
          TightDefinitionList.Definition(Seq(
            Text("definition")
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
    TightDefinitionList(Seq(
      TightDefinitionList.Item(
        DefinitionList.Term(Seq(Text("term"))),
        Seq(
          TightDefinitionList.Definition(Seq(
            Text("multi"), Space(), Text("line")
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
    TightDefinitionList(Seq(
      TightDefinitionList.Item(
        DefinitionList.Term(Seq(Text("term"))),
        Seq(
          TightDefinitionList.Definition(Seq(
            Text("multi"), Space(), Text("line")
          ))
        )
      )
    ))
  )

  parsing("""
  |term
  |~ definition
  """) as definitionList should produce (
    TightDefinitionList(Seq(
      TightDefinitionList.Item(
        DefinitionList.Term(Seq(
          Text("term")
        )),
        Seq(
          TightDefinitionList.Definition(Seq(
            Text("definition")
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
    TightDefinitionList(Seq(
      TightDefinitionList.Item(
        DefinitionList.Term(Seq(Text("term1"))),
        Seq(
          TightDefinitionList.Definition(Seq(Text("def1"))),
          TightDefinitionList.Definition(Seq(Text("def2")))
        )
      ),
      TightDefinitionList.Item(
        DefinitionList.Term(Seq(Text("term2"))),
        Seq(
          TightDefinitionList.Definition(Seq(Text("def1"))),
          TightDefinitionList.Definition(Seq(Text("def2")))
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
    LooseDefinitionList(Seq(
      LooseDefinitionList.Item(
        DefinitionList.Term(Seq(Text("term1"))),
        Seq(
          LooseDefinitionList.Definition(Seq(
            Paragraph(Seq(Text("def1")))
          )),
          LooseDefinitionList.Definition(Seq(
            Paragraph(Seq(Text("def2")))
          ))
        )
      ),
      LooseDefinitionList.Item(
        DefinitionList.Term(Seq(Text("term2"))),
        Seq(
          LooseDefinitionList.Definition(Seq(
            Paragraph(Seq(Text("def1")))
          )),
          LooseDefinitionList.Definition(Seq(
            Paragraph(Seq(Text("def2")))
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
    LooseDefinitionList(Seq(
      LooseDefinitionList.Item(
        DefinitionList.Term(Seq(Text("term"))),
        Seq(
          LooseDefinitionList.Definition(Seq(
            Paragraph(Seq(Text("def1")))
          )),
          LooseDefinitionList.Definition(Seq(
            Paragraph(Seq(Text("def2")))
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
    LooseDefinitionList(Seq(
      LooseDefinitionList.Item(
        DefinitionList.Term(Seq(Text("term"))),
        Seq(
          LooseDefinitionList.Definition(Seq(
            Paragraph(Seq(Text("def1")))
          )),
          LooseDefinitionList.Definition(Seq(
            Paragraph(Seq(Text("def2")))
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
    TightDefinitionList(Seq(
      TightDefinitionList.Item(
        DefinitionList.Term(Seq(Text("a"))),
        Seq(
          TightDefinitionList.Definition(
            Seq(Text("b")),
            Seq(
              TightUnorderedList(Seq(
                TightUnorderedList.Item(Seq(Text("c"))),
                TightUnorderedList.Item(Seq(Text("d")))
              ))
            )
          ),
          TightDefinitionList.Definition(
            Seq(Text("e"))
          )
        )
      ),
      TightDefinitionList.Item(
        DefinitionList.Term(Seq(Text("f"))),
        Seq(
          TightDefinitionList.Definition(
            Seq(Text("g"))
          )
        )
      )
    ))
  )
  */
}
