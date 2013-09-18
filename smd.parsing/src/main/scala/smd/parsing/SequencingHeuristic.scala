package smd
package parsing

/** Defines a heuristic for combining two parsers into a resultant parser which applies the two argument parsers
  * in sequence.
  *
  * @tparam L the type of the left-hand parser.
  * @tparam R the type of the right-hand parser.
  * @tparam S the type of the resultant sequence parser.
  */
trait SequencingHeuristic[-L <: Parser[_], -R <: Parser[_], +S <: Parser[_]] extends ((L, R) => S) {
  /** Combine the provided left and right-hand parsers into a single parser which parses them in sequence.
    *
    * @param lhs the left-hand parser.
    * @param rhs the right-hand parser.
    */
  def apply(lhs: L, rhs: R): S
}

object SequencingHeuristic extends Tier4SequencingHeuristics
                              with Tier3SequencingHeuristics
                              with Tier2SequencingHeuristics
                              with Tier1SequencingHeuristics
{ }

/** Tier 2 sequencing heuristics; defines a single [[smd.parsing.SequencingHeuristic]] which combines two
  * [[smd.parsing.SequenceParserLike]] instances into an [[smd.parsing.SequenceParser]] which is used in the event
  * that two strongly typed sequence parsers cannot be concatenated.
  */
trait Tier2SequencingHeuristics extends Tier3SequencingHeuristics {
  implicit val sequencingHeuristic_Seq_Seq: SequencingHeuristic[
    SequenceParserLike[_],
    SequenceParserLike[_],
    SequenceParser
  ] =
    create((l, r) => SequenceParser(l.sequence ++ r.sequence))
}

/** Tier 4 sequencing heuristics; defines the single base [[smd.parsing.SequencingHeuristic]] used for all sequences,
  * which combines two [[smd.parsing.Parser]] instances into an [[smd.parsing.SequenceParser2]].
  */
trait Tier4SequencingHeuristics {
  /** This function uses type inferral to eliminate a lot of the boilerplate which would otherwise be required for the
    * creation of a [[smd.parsing.SequencingHeuristic]].
    *
    * @param comb function implementing the concatenation heuristic.
    * @tparam L the left-hand parser type (inferred).
    * @tparam R the right-hand parser type (inferred).
    * @tparam S the combined parser type (inferred).
    * @return a [[smd.parsing.SequencingHeuristic]] implementation which delegates to the provided `comb` function.
    */
  def create[L <: Parser[_], R <: Parser[_], S <: Parser[_]](comb: (L, R) => S): SequencingHeuristic[L, R, S] =
    new SequencingHeuristic[L, R, S] { def apply(lhs: L, rhs: R): S = comb(lhs, rhs) }

  implicit def sequencingHeuristic_L_R[L, R]: SequencingHeuristic[
    /*  left: */ Parser[L],
    /* right: */ Parser[R],
    /*  dest: */ SequenceParser2[L, R]
  ] =
    create((l, r) => SequenceParser2(l, r))
}
