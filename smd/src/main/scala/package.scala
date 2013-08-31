package object smd {
  implicit class RichCharSequence(val cs: CharSequence) extends AnyVal {
    def proxySubSequence(start: Int, end: Int): CharSequence =
      new smd.util.ProxyCharSequence(cs, start, end)
  }

  implicit class RichSeq[A](val seq: collection.Seq[A]) extends AnyVal {
    def lengthEq(len: Int): Boolean  = seq.lengthCompare(len) == 0
    def lengthGt(len: Int): Boolean  = seq.lengthCompare(len) >  0
    def lengthGte(len: Int): Boolean = seq.lengthCompare(len) >= 0
    def lengthLt(len: Int): Boolean  = seq.lengthCompare(len) <  0
    def lengthLte(len: Int): Boolean = seq.lengthCompare(len) <= 0
  }

  implicit class RichString(val s: String) extends AnyVal {
    def literalEncode: String = util.LiteralEncoding.encode(s)
  }
}
