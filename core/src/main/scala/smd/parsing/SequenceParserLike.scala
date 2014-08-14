package smd
package parsing

trait SequenceParserLike[+A] extends Parser[A] {
  /** The sequence of parsers. */
  def sequence: IndexedSeq[Parser[Any]]
}
