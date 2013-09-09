package smd
package grammar

trait IdentifierExpressionProductions extends CommonExpressionProductions {
  lazy val IdentifierExpression: Parser[expression.Identifier] = Identifier ^* expression.Identifier

  protected lazy val Identifier: Parser[String] =
    // Not a keyword: not a keyword followed by something other than an identifier part
    !:(Keyword ~ !:(IdentifierExpressionPart)) ~>
    IdentifierExpressionStart ~ IdentifierExpressionPart.* ^* { case (s, ps) => (new StringBuilder(s) /: ps.flatten) { (sb, p) => sb.append(p) }.toString }

  /** A valid non-initial portion of an identifier. */
  protected lazy val IdentifierExpressionPart: Parser[String] = IdentPartCriteria ^* (_.charSequence.toString) | IdentPartEscape

  private lazy val IdentifierExpressionStart: Parser[String] = IdentStartCriteria ^* (_.charSequence.toString) | IdentStartEscape

  // Escape sequences are allowed in identifiers on the condition that the characters they introduce are valid
  // in the event that they are substituted for the escapes; thus here we filter escapes using the code point
  // criteria established for unescaped characters.

  private lazy val IdentPartEscape: Parser[String] =
    Escape ^*? {
      case cps if cps.forall(cp => IdentPartCriteria.isSatisfiedBy(cp)) => new String(cps.flatMap(Character.toChars(_)).toArray)
    }

  private lazy val IdentStartEscape: Parser[String] =
    Escape ^*? {
      case cps if IdentStartCriteria.isSatisfiedBy(cps.head) &&
                  cps.tail.forall(cp => IdentPartCriteria.isSatisfiedBy(cp))
        => new String(cps.flatMap(Character.toChars(_)).toArray)
    }

  private lazy val IdentPartCriteria =
    IdentStartCriteria || CodePoint.Category(UnicodeCategory.map(u => u.Mn + u.Mc + u.Nd + u.Pc)) || CodePoint.Values('\u200c', '\u200d')

  private lazy val IdentStartCriteria =
    CodePoint.Category(UnicodeCategory.map(u => u.Groups.L + u.Nl)) || CodePoint.Values('$', '_')
}
