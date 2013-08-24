package smd
package util

/** Facility for encoding & decoding hexadecimal values. */
object HexEncoding {
  /** Lookup table for encoding lowercase hexadecimal digits. */
  private val Lower = "0123456789abcdef".toCharArray

  /** Lookup table for encoding uppercase hexadecimal digits. */
  private val Upper = "0123456789ABCDEF".toCharArray

  /** Lookup table for decoding hexadecimal digits. */
  private val DecodeOption: Array[Option[Int]] = Array.fill('f'+1)(Option.empty[Int])
              "0123456789AaBbCcDdEeFf".foreach { c =>
                DecodeOption(c) = Some((0x0F & c) + ((0x40 & c) >>> 6) * 9)
              }

  // There is no reason to implement additional overloads of these methods, as integer conversion
  // is required to serve as an array offset.

  /** Retrieve the lowercase hexadecimal digit representation of the four least significant bits of the provided `value`.
    *
    * @param value the value whose four least significant bits are to be encoded as an lowercase hexadecimal digit.
    * @return the lowercase hexadecimal digit representation of the four least significant bits of the provided `value`.
    */
  @inline def encodeDigitLower(value: Int) = Lower(value & 0x0F)

  /** Retrieve the uppercase hexadecimal digit representation of the four least significant bits of the provided `value`.
    *
    * @param value the value whose four least significant bits are to be encoded as an uppercase hexadecimal digit.
    * @return the uppercase hexadecimal digit representation of the four least significant bits of the provided `value`.
    */
  @inline def encodeDigitUpper(value: Int) = Upper(value & 0x0F)

  /** Retrive the value associated with the provided hexadecimal `digit` representation.
    *
    * @param digit the digit for which the associated value should be retrieved.
    * @return `Some(value)` if `digit` is a valid hexadecimal digit, `None` otherwise.
    */
  @inline def decodeDigit(digit: Char) = if(digit <= 'f') DecodeOption(digit) else None

  /** Retrieve the lowercase hexadecimal string representation of the provided value.
    *
    * @param value the value to be converted to its lowerwase hexadecimal string representation.
    * @return the equivalent hexadecimal string representation of `value`.
    */
  @inline def encodeLower(value: Byte): String = encodeLower(value, new StringBuilder(2)).toString

  /** Retrieve the uppercase hexadecimal string representation of the provided value.
    *
    * @param value the value to be converted to its uppercase hexadecimal string representation.
    * @return the equivalent hexadecimal string representation of `value`.
    */
  @inline def encodeUpper(value: Byte): String = encodeUpper(value, new StringBuilder(2)).toString

  /** Retrieve the lowercase hexadecimal string representation of the provided value.
    *
    * @param value the value to be converted to its lowerwase hexadecimal string representation.
    * @return the equivalent hexadecimal string representation of `value`.
    */
  @inline def encodeLower(value: Short): String = encodeLower(value, new StringBuilder(4)).toString

  /** Retrieve the uppercase hexadecimal string representation of the provided value.
    *
    * @param value the value to be converted to its uppercase hexadecimal string representation.
    * @return the equivalent hexadecimal string representation of `value`.
    */
  @inline def encodeUpper(value: Short): String = encodeUpper(value, new StringBuilder(4)).toString

  /** Retrieve the lowercase hexadecimal string representation of the provided value.
    *
    * @param value the value to be converted to its lowerwase hexadecimal string representation.
    * @return the equivalent hexadecimal string representation of `value`.
    */
  @inline def encodeLower(value: Int): String = encodeLower(value, new StringBuilder(8)).toString

  /** Retrieve the uppercase hexadecimal string representation of the provided value.
    *
    * @param value the value to be converted to its uppercase hexadecimal string representation.
    * @return the equivalent hexadecimal string representation of `value`.
    */
  @inline def encodeUpper(value: Int): String = encodeUpper(value, new StringBuilder(8)).toString

  /** Retrieve the lowercase hexadecimal string representation of the provided value.
    *
    * @param value the value to be converted to its lowerwase hexadecimal string representation.
    * @return the equivalent hexadecimal string representation of `value`.
    */
  @inline def encodeLower(value: Long): String = encodeLower(value, new StringBuilder(16)).toString

  /** Retrieve the uppercase hexadecimal string representation of the provided value.
    *
    * @param value the value to be converted to its uppercase hexadecimal string representation.
    * @return the equivalent hexadecimal string representation of `value`.
    */
  @inline def encodeUpper(value: Long): String = encodeUpper(value, new StringBuilder(16)).toString

  /** Append the lowercase hexadecimal string representation of the provided value to the provided [[scala.collection.mutable.StringBuilder]].
    *
    * @param value the value whose hexadecimal string representation is to be appended to the [[scala.collection.mutable.StringBuilder]].
    * @param sb the [[scala.collection.mutable.StringBuilder]] to which the hexadecimal encoding should be appended.
    * @return the provided [[scala.collection.mutable.StringBuilder]], sb.
    */
  @inline def encodeLower(value: Byte, sb: StringBuilder) =
    sb.append(encodeDigitLower(value >>> 4))
      .append(encodeDigitLower(value))

  /** Append the uppercase hexadecimal string representation of the provided value to the provided [[scala.collection.mutable.StringBuilder]].
    *
    * @param value the value whose hexadecimal string representation is to be appended to the [[scala.collection.mutable.StringBuilder]].
    * @param sb the [[scala.collection.mutable.StringBuilder]] to which the hexadecimal encoding should be appended.
    * @return the provided [[scala.collection.mutable.StringBuilder]], sb.
    */
  @inline def encodeUpper(value: Byte, sb: StringBuilder) =
    sb.append(encodeDigitUpper(value >>> 4))
      .append(encodeDigitUpper(value))

  /** Append the lowercase hexadecimal string representation of the provided value to the provided [[scala.collection.mutable.StringBuilder]].
    *
    * @param value the value whose hexadecimal string representation is to be appended to the [[scala.collection.mutable.StringBuilder]].
    * @param sb the [[scala.collection.mutable.StringBuilder]] to which the hexadecimal encoding should be appended.
    * @return the provided [[scala.collection.mutable.StringBuilder]], sb.
    */
  @inline def encodeLower(value: Short, sb: StringBuilder) =
    sb.append(encodeDigitLower(value >>> 12))
      .append(encodeDigitLower(value >>>  8))
      .append(encodeDigitLower(value >>>  4))
      .append(encodeDigitLower(value))

  /** Append the uppercase hexadecimal string representation of the provided value to the provided [[scala.collection.mutable.StringBuilder]].
    *
    * @param value the value whose hexadecimal string representation is to be appended to the [[scala.collection.mutable.StringBuilder]].
    * @param sb the [[scala.collection.mutable.StringBuilder]] to which the hexadecimal encoding should be appended.
    * @return the provided [[scala.collection.mutable.StringBuilder]], sb.
    */
  @inline def encodeUpper(value: Short, sb: StringBuilder) =
    sb.append(encodeDigitUpper(value >>> 12))
      .append(encodeDigitUpper(value >>>  8))
      .append(encodeDigitUpper(value >>>  4))
      .append(encodeDigitUpper(value))

  /** Append the lowercase hexadecimal string representation of the provided value to the provided [[scala.collection.mutable.StringBuilder]].
    *
    * @param value the value whose hexadecimal string representation is to be appended to the [[scala.collection.mutable.StringBuilder]].
    * @param sb the [[scala.collection.mutable.StringBuilder]] to which the hexadecimal encoding should be appended.
    * @return the provided [[scala.collection.mutable.StringBuilder]], sb.
    */
  @inline def encodeLower(value: Int, sb: StringBuilder) =
    sb.append(encodeDigitLower(value >>> 28))
      .append(encodeDigitLower(value >>> 24))
      .append(encodeDigitLower(value >>> 20))
      .append(encodeDigitLower(value >>> 16))
      .append(encodeDigitLower(value >>> 12))
      .append(encodeDigitLower(value >>>  8))
      .append(encodeDigitLower(value >>>  4))
      .append(encodeDigitLower(value))

  /** Append the uppercase hexadecimal string representation of the provided value to the provided [[scala.collection.mutable.StringBuilder]].
    *
    * @param value the value whose hexadecimal string representation is to be appended to the [[scala.collection.mutable.StringBuilder]].
    * @param sb the [[scala.collection.mutable.StringBuilder]] to which the hexadecimal encoding should be appended.
    * @return the provided [[scala.collection.mutable.StringBuilder]], sb.
    */
  @inline def encodeUpper(value: Int, sb: StringBuilder) =
    sb.append(encodeDigitUpper(value >>> 28))
      .append(encodeDigitUpper(value >>> 24))
      .append(encodeDigitUpper(value >>> 20))
      .append(encodeDigitUpper(value >>> 16))
      .append(encodeDigitUpper(value >>> 12))
      .append(encodeDigitUpper(value >>>  8))
      .append(encodeDigitUpper(value >>>  4))
      .append(encodeDigitUpper(value))

  /** Append the lowercase hexadecimal string representation of the provided value to the provided [[scala.collection.mutable.StringBuilder]].
    *
    * @param value the value whose hexadecimal string representation is to be appended to the [[scala.collection.mutable.StringBuilder]].
    * @param sb the [[scala.collection.mutable.StringBuilder]] to which the hexadecimal encoding should be appended.
    * @return the provided [[scala.collection.mutable.StringBuilder]], sb.
    */
  @inline def encodeLower(value: Long, sb: StringBuilder) =
    sb.append(encodeDigitLower((value >>> 60).toInt))
      .append(encodeDigitLower((value >>> 56).toInt))
      .append(encodeDigitLower((value >>> 52).toInt))
      .append(encodeDigitLower((value >>> 48).toInt))
      .append(encodeDigitLower((value >>> 44).toInt))
      .append(encodeDigitLower((value >>> 40).toInt))
      .append(encodeDigitLower((value >>> 36).toInt))
      .append(encodeDigitLower((value >>> 32).toInt))
      .append(encodeDigitLower((value >>> 28).toInt))
      .append(encodeDigitLower((value >>> 24).toInt))
      .append(encodeDigitLower((value >>> 20).toInt))
      .append(encodeDigitLower((value >>> 16).toInt))
      .append(encodeDigitLower((value >>> 12).toInt))
      .append(encodeDigitLower((value >>>  8).toInt))
      .append(encodeDigitLower((value >>>  4).toInt))
      .append(encodeDigitLower((value).toInt))

  /** Append the uppercase hexadecimal string representation of the provided value to the provided [[scala.collection.mutable.StringBuilder]].
    *
    * @param value the value whose hexadecimal string representation is to be appended to the [[scala.collection.mutable.StringBuilder]].
    * @param sb the [[scala.collection.mutable.StringBuilder]] to which the hexadecimal encoding should be appended.
    * @return the provided [[scala.collection.mutable.StringBuilder]], sb.
    */
  @inline def encodeUpper(value: Long, sb: StringBuilder) =
    sb.append(encodeDigitUpper((value >>> 60).toInt))
      .append(encodeDigitUpper((value >>> 56).toInt))
      .append(encodeDigitUpper((value >>> 52).toInt))
      .append(encodeDigitUpper((value >>> 48).toInt))
      .append(encodeDigitUpper((value >>> 44).toInt))
      .append(encodeDigitUpper((value >>> 40).toInt))
      .append(encodeDigitUpper((value >>> 36).toInt))
      .append(encodeDigitUpper((value >>> 32).toInt))
      .append(encodeDigitUpper((value >>> 28).toInt))
      .append(encodeDigitUpper((value >>> 24).toInt))
      .append(encodeDigitUpper((value >>> 20).toInt))
      .append(encodeDigitUpper((value >>> 16).toInt))
      .append(encodeDigitUpper((value >>> 12).toInt))
      .append(encodeDigitUpper((value >>>  8).toInt))
      .append(encodeDigitUpper((value >>>  4).toInt))
      .append(encodeDigitUpper((value).toInt))
}
