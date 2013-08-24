package object smd {
  implicit class RichString(val s: String) extends AnyVal {
    def literalEncode: String = util.LiteralEncoding.encode(s)
  }
}
