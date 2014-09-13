package smd
package parsing

import smd.util.Trie

/** [[smd.parsing.Parser]] implementation which provides high-performance matching of a set of literal strings using
  * longest-match semantics. The literals to be matched are provided as a [[scala.collection.immutable.Map]] mapping
  * literals to corresponding products.
  *
  * Internally matching is implemented using a trie data structure, allowing rapid matching of extremely large sets.
  *
  * @param literals a mapping from literals to be matched to their associated products.
  * @tparam A the product type.
  */
case class LiteralSetParser[+A](literals: Map[String, A]) extends Parser[A] {
  require(literals.nonEmpty, "provided literal mapping cannot be empty")

  protected val trie = Trie(literals)

  def parse(context: ParsingContext): ParsingResult[A] = {
    // There is no point to an implementation with shortest match semantics, as if a prefix of another longer
    // were provided, the longer literal could never be matched.
    def longestMatch(trie: Trie[A], i: Int): Option[(A, Int)] = {
      @inline def current = if(trie.value.isDefined) Some((trie.value.get, i)) else None

      if(context.length == i) current
      else {
        val subtrie = trie.subtrie(context.input.charAt(i))
        if(subtrie.isEmpty) current
        else {
          val longer = longestMatch(subtrie.get, i + 1)
          if(longer.isDefined) longer else current
        }
      }
    }

    val rb = context.resultBuilder
    longestMatch(trie, context.index) match {
      case None => rb.reject
      case Some((product, matchEnd)) =>
        if(matchEnd == context.index || matchEnd == context.graphemeAt(matchEnd - 1).end) {
          context.advanceTo(matchEnd)
          rb.accept(product)
        } else {
          rb.reject
        }
    }
  }
}

object LiteralSetParser {
  /** Create a new [[smd.parsing.LiteralSetParser]] matching the provided literals.
    *
    * @param lit0 a literal to be matched by the resulting [[smd.parsing.LiteralSetParser]].
    * @param lit1 a literal to be matched by the resulting [[smd.parsing.LiteralSetParser]].
    * @param literals additional literals to be matched by the resulting [[smd.parsing.LiteralSetParser]].
    */
  def apply(lit0: String, lit1: String, literals: String*): LiteralSetParser[String] =
    apply((lit0 +: lit1 +: literals).map(x => (x, x)))

  /** Create a new [[smd.parsing.LiteralSetParser]] matching the provided literals.
    *
    * @param lit0 a literal and associated product to be matched by the resulting [[smd.parsing.LiteralSetParser]].
    * @param lit1 a literal and associated product to be matched by the resulting [[smd.parsing.LiteralSetParser]].
    * @param literals additional literals and associated products to be matched by the resulting [[smd.parsing.LiteralSetParser]].
    * @tparam A the product type.
    */
  def apply[A](lit0: (String, A), lit1: (String, A), literals: (String, A)*): LiteralSetParser[A] =
    apply(lit0 +: lit1 +: literals)

  /** Create a new [[smd.parsing.LiteralSetParser]] matching the provided literals.
    *
    * @param literals literals and associated products to be matched by the resulting [[smd.parsing.LiteralSetParser]].
    * @tparam A the product type.
    */
  def apply[A](literals: Iterable[(String, A)]): LiteralSetParser[A] =
    apply(literals.toMap)

  /** Create a new [[smd.parsing.LiteralSetParser]] matching the provided literals with the matched
    * literal as its product.
    * 
    * @param literals The literals to be matched by the resulting [[smd.parsing.LiteralSetParser]].
    */
  def identity(literals: Iterable[String]): LiteralSetParser[String] = apply(literals.map(s => (s, s)).toMap)
}
