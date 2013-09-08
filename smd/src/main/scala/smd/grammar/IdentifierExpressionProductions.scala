package smd
package grammar

trait IdentifierExpressionProductions extends CommonExpressionProductions {
  lazy val IdentifierExpression = Identifier ^* { p => expression.Identifier(p) }

  // TODO: Decode identifier name
  lazy val Identifier = !:(Keyword ~ !:(IdentifierExpressionPart)) ~
                        IdentifierExpressionStart ~ IdentifierExpressionPart.* ^^ { r => r.parsed.toString }

  lazy val IdentifierExpressionPart = IdentifierExpressionStart |
                                      CodePoint.Category(UnicodeCategory.map(u => u.Mn + u.Mc + u.Nd + u.Pc)) |
                                      "\u200C" | "\u200D"

  lazy val IdentifierExpressionStart = CodePoint.Category(UnicodeCategory.map(u => u.Groups.L + u.Nl)) | "$" | "_" | "\\" ~ UnicodeEscapeSequence

}
