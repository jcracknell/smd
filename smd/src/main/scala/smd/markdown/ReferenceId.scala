package smd
package markdown

final class ReferenceId(val raw: String) {
  /** The normalized string value of this reference id. */
  val normalized: String = ReferenceId.normalize(raw)

  override def equals(obj: scala.Any): Boolean = obj match {
    case that: ReferenceId => this.normalized == that.normalized
    case _ => false
  }

  override def hashCode(): Int = normalized.hashCode

  override def toString: String = s"${getClass.getName}(${normalized.literalEncode})"
}

object ReferenceId {
  def apply(raw: String): ReferenceId = new ReferenceId(raw)

  /** Converts the provided raw string to the corresponding normalized reference id string. */
  def normalize(raw: String): String =
    java.text.Normalizer.normalize(raw, java.text.Normalizer.Form.NFKD)
    .map(c => if(Character.isAlphabetic(c) || Character.isDigit(c)) c else ' ').trim
    .map(Character.toLowerCase)
    .replaceAll("\\s+", "-")
}
