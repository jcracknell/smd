package smd
package parsing

trait Parsers {
  implicit def string2LiteralParser(str: String): LiteralParser = LiteralParser(str)
}
