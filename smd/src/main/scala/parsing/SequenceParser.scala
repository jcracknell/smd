package smd
package parsing

case class SequenceParser(sequence: IndexedSeq[Parser[Any]]) extends Parser[IndexedSeq[Any]] {
  require(sequence.lengthGt(2), "sequence must contain at least two parsers.")

  def parse(context: ParsingContext): ParsingResult[IndexedSeq[Any]] = {
    val rb = context.resultBuilder
    val products = Array.ofDim[Any](sequence.length)
    var i = 0
    while(sequence.length != i) {
      val r = sequence(i).parse(context)
      if(r.succeeded){
        products(i) = r.product
        i += 1
      } else {
        return rb.failure
      }
    }

    rb.success(products)
  }
}

object SequenceParser {
  def apply(p0: Parser[Any], pns: Parser[Any]*): SequenceParser =
    SequenceParser((p0 +: pns).toIndexedSeq)

  implicit val sequencingHeuristic: SequencingHeuristic[SequenceParser, Parser[_], SequenceParser] =
    SequencingHeuristic.create((l, r) => SequenceParser(l.sequence :+ r))
}
