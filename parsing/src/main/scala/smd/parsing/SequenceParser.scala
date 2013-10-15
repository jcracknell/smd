package smd
package parsing

case class SequenceParser(sequence: IndexedSeq[Parser[Any]]) extends SequenceParserLike[IndexedSeq[Any]] {
  require(sequence.lengthGte(2), "sequence must contain at least two parsers.")

  def parse(context: ParsingContext): ParsingResult[IndexedSeq[Any]] = {
    val rb = context.resultBuilder
    val products = Array.ofDim[Any](sequence.length)
    var i = 0

    do {
      val r = sequence(i).parse(context)
      if(r.accepted){
        products(i) = r.product
      } else {
        return rb.reject
      }

      i += 1
    } while(i < sequence.length)

    rb.accept(products)
  }
}

object SequenceParser {
  def apply(p0: Parser[Any], pns: Parser[Any]*): SequenceParser =
    SequenceParser((p0 +: pns).toIndexedSeq)
}
