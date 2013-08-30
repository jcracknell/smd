package object smd {
  implicit class RichCharSequence(val cs: CharSequence) extends AnyVal {
    def proxySubSequence(start: Int, end: Int): CharSequence =
      new smd.util.ProxyCharSequence(cs, start, end)
  }
  implicit class RichString(val s: String) extends AnyVal {
    def literalEncode: String = util.LiteralEncoding.encode(s)
  }
}
