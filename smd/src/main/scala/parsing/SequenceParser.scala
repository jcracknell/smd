package smd
package parsing

case class SequenceParser(parsers: Parser[Any]*) extends Parser[IndexedSeq[Any]] {
  def parse(context: ParsingContext): ParsingResult[IndexedSeq[Any]] = {
    val mark = context.mark
    val products = Array.ofDim[Any](parsers.length)
    var i = 0
    while(parsers.length != i) {
      val r = parsers(i).parse(context)
      if(r.succeeded){
        products(i) = r.product
        i += 1
      } else {
        return Failure
      }
    }

    mark.success(products)
  }
}