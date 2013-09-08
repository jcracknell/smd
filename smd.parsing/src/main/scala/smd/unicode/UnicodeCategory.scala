package smd
package unicode

/** Algebraic type modelling a unicode character category as defined in the unicode standard.
  * The purpose of this class is to improve discoverability and enable implicit conversions while retaining
  * interoperability with Java's unicode facilities.
  *
  * @param abbr the abbreviation assigned to the character category.
  * @param value the value assigned to the character category by Java's unicode facilities.
  */
sealed abstract class UnicodeCategory(val abbr: String, val value: Byte) {
  /** Creates a set consisting of this category and the provided other category. */
  def +(other: UnicodeCategory): Set[UnicodeCategory] = Set(this, other)
  override def toString: String = abbr
}

/** Facility defining [[smd.unicode.UnicodeCategory]] values.
  *
  * @define category category defined in the unicode standard.
  *
  * @define Cc    The "Control" or "Cc" $category
  * @define Cf    The "Format" or "Cf" $category
  * @define Cn    The "Unassigned" or "Cn" $category
  * @define Co    The "Private Use" or "Co" $category
  * @define Cs    The "Surrogate" or "Cs" $category
  * @define Ll    The "Lowercase Letter" or "Ll" $category
  * @define Lm    The "Modifier Letter" or "Lm" $category
  * @define Lo    The "Other Letter" or "Lo" $category
  * @define Lt    The "Titlecase Letter" or "Lt" $category
  * @define Lu    The "Uppercase Letter" or "Lu" $category
  * @define Mc    The "Spacing Mark" or "Mc" $category
  * @define Me    The "Enclosing Mark" or "Me" $category
  * @define Mn    The "Non-spacing Mark" or "Mn" $category
  * @define Nd    The "Decmal Number" or "Nd" $category
  * @define Nl    The "Letter Number" or "Nl" $category
  * @define No    The "Other Number" or "No" $category
  * @define Pc    The "Connector Punctuation" or "Pc" $category
  * @define Pd    The "Dash Punctuation" or "Pd" $category
  * @define Pe    The "Close Punctuation" or "Pe" $category
  * @define Pf    The "Final Punctuation" or "Pf" $category
  * @define Pi    The "Initial Punctuation" or "Pi" $category
  * @define Po    The "Other Punctuation" or "Po" $category
  * @define Ps    The "Open Punctuation" or "Ps" $category
  * @define Sc    The "Currency Symbol" or "Sc" $category
  * @define Sk    The "Modifier Symbol" or "Sk" $category
  * @define Sm    The "Math Symbol" or "Sm" $category
  * @define So    The "Other Symbol" or "So" $category
  * @define Zl    The "Line Separator" or "Zl" $category
  * @define Zp    The "Paragraph Separator" or "Zp" $category
  * @define Zs    The "Space Separator" or "Zs" $category
  */
object UnicodeCategory {
  /** Convenience method for extracting category values:
    *
    * {{{
    * UnicodeCategory.map(u => u.Ll + u.Lu + u.Lt)
    * }}}
    *
    * @param op the map operation
    * @tparam A the result type
    * @return the result of applying the map operation
    */
  def map[A](op: UnicodeCategory.type => A): A = op(UnicodeCategory)

  /** $Cc */ case object Control              extends UnicodeCategory("Cc", Character.CONTROL)
  /** $Cc */    val Cc = Control
  /** $Cf */ case object Format               extends UnicodeCategory("Cf", Character.FORMAT)
  /** $Cf */    val Cf = Format
  /** $Cn */ case object Unassigned           extends UnicodeCategory("Cn", Character.UNASSIGNED)
  /** $Cn */    val Cn = Unassigned
  /** $Co */ case object PrivateUse           extends UnicodeCategory("Co", Character.PRIVATE_USE)
  /** $Co */    val Co = PrivateUse
  /** $Cs */ case object Surrogate            extends UnicodeCategory("Cs", Character.SURROGATE)
  /** $Cs */    val Cs = Surrogate
  /** $Ll */ case object LowercaseLetter      extends UnicodeCategory("Ll", Character.LOWERCASE_LETTER)
  /** $Ll */    val Ll = LowercaseLetter
  /** $Lm */ case object ModifierLetter       extends UnicodeCategory("Lm", Character.MODIFIER_LETTER)
  /** $Lm */    val Lm = ModifierLetter
  /** $Lo */ case object OtherLetter          extends UnicodeCategory("Lo", Character.OTHER_LETTER)
  /** $Lo */    val Lo = OtherLetter
  /** $Lt */ case object TitlecaseLetter      extends UnicodeCategory("Lt", Character.TITLECASE_LETTER)
  /** $Lt */    val Lt = TitlecaseLetter
  /** $Lu */ case object UppercaseLetter      extends UnicodeCategory("Lu", Character.UPPERCASE_LETTER)
  /** $Lu */    val Lu = UppercaseLetter
  /** $Mc */ case object SpacingMark          extends UnicodeCategory("Mc", Character.COMBINING_SPACING_MARK)
  /** $Mc */    val Mc = SpacingMark
  /** $Me */ case object EnclosingMark        extends UnicodeCategory("Me", Character.ENCLOSING_MARK)
  /** $Me */    val Me = EnclosingMark
  /** $Mn */ case object NonSpacingMark       extends UnicodeCategory("Mn", Character.NON_SPACING_MARK)
  /** $Mn */    val Mn = NonSpacingMark
  /** $Nd */ case object DecimalNumber        extends UnicodeCategory("Nd", Character.DECIMAL_DIGIT_NUMBER)
  /** $Nd */    val Nd = DecimalNumber
  /** $Nl */ case object LetterNumber         extends UnicodeCategory("Nl", Character.LETTER_NUMBER)
  /** $Nl */    val Nl = LetterNumber
  /** $No */ case object OtherNumber          extends UnicodeCategory("No", Character.OTHER_NUMBER)
  /** $No */    val No = OtherNumber
  /** $Pc */ case object ConnectorPunctuation extends UnicodeCategory("Pc", Character.CONNECTOR_PUNCTUATION)
  /** $Pc */    val Pc = ConnectorPunctuation
  /** $Pd */ case object DashPunctuation      extends UnicodeCategory("Pd", Character.DASH_PUNCTUATION)
  /** $Pd */    val Pd = DashPunctuation
  /** $Pe */ case object ClosePunctuation     extends UnicodeCategory("Pe", Character.END_PUNCTUATION)
  /** $Pe */    val Pe = ClosePunctuation
  /** $Pf */ case object FinalPunctuation     extends UnicodeCategory("Pf", Character.FINAL_QUOTE_PUNCTUATION)
  /** $Pf */    val Pf = FinalPunctuation
  /** $Pi */ case object InitialPunctuation   extends UnicodeCategory("Pi", Character.INITIAL_QUOTE_PUNCTUATION)
  /** $Pi */    val Pi = InitialPunctuation
  /** $Po */ case object OtherPunctuation     extends UnicodeCategory("Po", Character.OTHER_PUNCTUATION)
  /** $Po */    val Po = OtherPunctuation
  /** $Ps */ case object OpenPunctuation      extends UnicodeCategory("Ps", Character.START_PUNCTUATION)
  /** $Ps */    val Ps = OpenPunctuation
  /** $Sc */ case object CurrencySymbol       extends UnicodeCategory("Sc", Character.CURRENCY_SYMBOL)
  /** $Sc */    val Sc = CurrencySymbol
  /** $Sk */ case object ModifierSymbol       extends UnicodeCategory("Sk", Character.MODIFIER_SYMBOL)
  /** $Sk */    val Sk = ModifierSymbol
  /** $Sm */ case object MathSymbol           extends UnicodeCategory("Sm", Character.MATH_SYMBOL)
  /** $Sm */    val Sm = MathSymbol
  /** $So */ case object OtherSymbol          extends UnicodeCategory("So", Character.OTHER_SYMBOL)
  /** $So */    val So = OtherSymbol
  /** $Zl */ case object LineSeparator        extends UnicodeCategory("Zl", Character.LINE_SEPARATOR)
  /** $Zl */    val Zl = LineSeparator
  /** $Zp */ case object ParagraphSeparator   extends UnicodeCategory("Zp", Character.PARAGRAPH_SEPARATOR)
  /** $Zp */    val Zp = ParagraphSeparator
  /** $Zs */ case object SpaceSeparator       extends UnicodeCategory("Zs", Character.SPACE_SEPARATOR)
  /** $Zs */    val Zs = SpaceSeparator

  /** Category groups defined in the unicode specification.
    *
    * @define categoryGroup category group defined in the unicode standard, equivalent to
    *
    * @define L    The "Letter" or "L" $categoryGroup `Lu | Ll | Lt | Lm | Lo`.
    * @define LC   The "Cased Letter" or "LC" $categoryGroup `Lu | Ll | Lt`.
    * @define C    The "Other" or "C" $categoryGroup `Cc | Cf | Cs | Co | Cn`.
    * @define P    The "Punctuation" or "P" $categoryGroup `Pc | Pd | Ps | Pi | Pf | Po`.
    * @define M    The "Mark" or "M" $categoryGroup `Mn | Mc | Me`.
    * @define N    The "Number" or "N" $categoryGroup `Nd | Nl | No`.
    * @define S    The "Symbol" or "S" $categoryGroup `Sm | Sc | Sk | So`.
    * @define Z    The "Separator" or "Z" $categoryGroup `Zs | Zl | Zp`.
    */
  object Groups {
    /** $L  */ val Letter: Set[UnicodeCategory]  =      Set(Lu, Ll, Lt, Lm, Lo)
    /** $L  */ val L = Letter
    /** $LC */ val CasedLetter: Set[UnicodeCategory] =  Set(Lu, Ll, Lt)
    /** $LC */ val LC = CasedLetter
    /** $C  */ val Control: Set [UnicodeCategory] =     Set(Cc, Cf, Cs, Co, Cn)
    /** $C  */ val C = Control
    /** $P  */ val Punctuation: Set [UnicodeCategory] = Set(Pc, Pd, Ps, Pe, Pi, Pf, Po)
    /** $P  */ val P = Punctuation
    /** $M  */ val Mark: Set [UnicodeCategory] =        Set(Mn, Mc, Me)
    /** $M  */ val M = Mark
    /** $N  */ val Number: Set [UnicodeCategory] =      Set(Nd, Nl, No)
    /** $N  */ val N = Number
    /** $S  */ val Symbol: Set [UnicodeCategory] =      Set(Sm, Sc, Sk, So)
    /** $S  */ val S = Symbol
    /** $Z  */ val Separator: Set [UnicodeCategory] =   Set(Zs, Zl, Zp)
    /** $Z  */ val Z = Separator
  }

  /** The set of all [[smd.unicode.UnicodeCategory]] values. */
  val AllCategories: Set[UnicodeCategory] = Set(Lu, Ll, Lt, Lm, Lo, Mn, Mc, Me, Nd, Nl, No, Pc, Pd, Ps, Pe,
                                                Pi, Pf, Po, Sm, Sc, Sk, So, Zs, Zl, Zp, Cc, Cf, Cs, Co, Cn)

  /** The maximum value of a category. */
  val MaxValue = AllCategories.map(_.value).max

  private val categoryLookup: Array[UnicodeCategory] = {
    val lut = Array.ofDim[UnicodeCategory](MaxValue + 1)
    for(c <- AllCategories)
      lut(c.value) = c
    lut
  }

  /** Retrieve the [[smd.unicode.UnicodeCategory]] value corresponding to the provided category value as defined by
    * Java's [[java.lang.Character]] class.
    *
    * @param category the category value
    */
  def get(category: Byte): UnicodeCategory = {
    require(0 <= category && category <= MaxValue, s"the provided category ($category) is invalid.")
    categoryLookup(category)
  }

  /** Retrieve the [[smd.unicode.UnicodeCategory]] value for the provided UTF-16 character. */
  def get(character: Char): UnicodeCategory = get(Character.getType(character))

  /** Retrieve the [[smd.unicode.UnicodeCategory]] value for the provided 32-bit code point. */
  def get(codePoint: Int): UnicodeCategory = get(Character.getType(codePoint))
}
