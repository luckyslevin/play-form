package ejisan.play.data

import play.api.data._
import play.api.data.Forms._
import play.api.data.format.Formats._
import play.api.data.format.Formatter
import play.api.data.validation._
import org.joda.time.{DateTime, LocalDate}

import Formats._

object Forms {

  /**
   * Constructs a simple mapping for a numeric year field (using a Int type behind).
   *
   * For example:
   * {{{
   * Form("year" -> yearNumber)
   * }}}
   */
  final val yearNumber: Mapping[Int] = number(min=0)

  /**
   * Constructs a simple mapping for a numeric month field (using a Int type behind).
   *
   * For example:
   * {{{
   * Form("month" -> monthNumber)
   * }}}
   */
  final val monthNumber: Mapping[Int] = number(min=1, max=12)

  /**
   * Constructs a simple mapping for a numeric day field (using a Int type behind).
   *
   * For example:
   * {{{
   * Form("day" -> dayNumber)
   * }}}
   */
  final val dayNumber: Mapping[Int] = number(min=1, max=31)

  /**
   * Constructs a simple mapping for a numeric hour field (using a Int type behind).
   *
   * For example:
   * {{{
   * Form("hour" -> monthNumber)
   * }}}
   */
  final val hourNumber: Mapping[Int] = number(min=0, max=23)

  /**
   * Constructs a simple mapping for a numeric minute field (using a Int type behind).
   *
   * For example:
   * {{{
   * Form("minute" -> minuteNumber)
   * }}}
   */
  final val minuteNumber: Mapping[Int] = number(min=0, max=59)

  /**
   * Constructs a simple mapping for a numeric second field (using a Int type behind).
   *
   * For example:
   * {{{
   * Form("second" -> secondNumber)
   * }}}
   */
  final val secondNumber: Mapping[Int] = number(min=0, max=59)

  def separatedJodaLocalDate(year: (String, Mapping[Int]), month: (String, Mapping[Int]), day: (String, Mapping[Int])): Mapping[LocalDate] = {
    mapping(year, month, day)({
      (year: Int, month: Int, day: Int) => new LocalDate(year, month, day)
    })({
      ld => Some((ld.year.get, ld.monthOfYear.get, ld.dayOfMonth.get))
    })
  }

  def separatedJodaDateTime(
    year: (String, Mapping[Int]),
    month: (String, Mapping[Int]),
    day: (String, Mapping[Int]),
    hour: (String, Mapping[Int]),
    minit: (String, Mapping[Int]),
    second: (String, Mapping[Int])
  ): Mapping[DateTime] = {
    mapping(year, month, day, hour, minit, second)({
      (year: Int, month: Int, day: Int, hour: Int, minit: Int, second: Int) =>
        new DateTime(year, month, day, hour, minit, second)
    })({
      dt => Some((
        dt.year.get,
        dt.monthOfYear.get,
        dt.dayOfMonth.get,
        dt.getHourOfDay,
        dt.getMinuteOfHour,
        dt.getSecondOfMinute
      ))
    })
  }

  def regexVerifying(regex: String, constraint: String, error: String) = {
    Constraint[String](constraint)({ test: String =>
      if (regex.r.unapplySeq(test).isDefined) Valid
      else Invalid(ValidationError(error))
    })
  }

  def emptyOrRegexVerifying(regex: String, constraint: String, error: String) = {
    Constraint[String](constraint)({ test: String =>
      if (test.isEmpty || regex.r.unapplySeq(test).isDefined) Valid
      else Invalid(ValidationError(error))
    })
  }

  /** matching */
  def matching[T](s1: (String, Mapping[T]), s2: (String, Mapping[T])): Mapping[T] = {
    tuple(s1, s2).verifying(Constraint[(T, T)]("constraint.match")({ s: (T, T) =>
      if (s._1 != s._2) Invalid(ValidationError("error.unmatched")) else Valid
    })).transform({ case (s1, _) => s1 }, (s1: T) => (s1, s1))
  }

  /** Alphabetical format */
  val alphabeticalText = text.verifying(emptyOrRegexVerifying(
    """^[a-zA-Z]+$""",
    "constraint.alphabetical",
    "error.invalid"
  ))
  /** Non-Alphabetical format */
  val nonEmptyAlphabeticalText = nonEmptyText.verifying(regexVerifying(
    """^[a-zA-Z]+$""",
    "constraint.alphabetical",
    "error.invalid"
  ))

  /** Symbolical format */
  val symbolicalText = text.verifying(emptyOrRegexVerifying(
    """^[ -/:-@\[-\`\{-\~]+$""",
    "constraint.symbolical",
    "error.invalid"
  ))
  /** Non-Symbolical URL format */
  val nonEmptySymbolicalText = nonEmptyText.verifying(regexVerifying(
    """^[ -/:-@\[-\`\{-\~]+$""",
    "constraint.symbolical",
    "error.invalid"
  ))

  /** Digit format */
  val digit = text.verifying(emptyOrRegexVerifying(
    """^[0-9]+$""",
    "constraint.digit",
    "error.invalid"
  ))
  /** Non-Digit URL format */
  val nonEmptyDigit = nonEmptyText.verifying(regexVerifying(
    """^[0-9]+$""",
    "constraint.digit",
    "error.invalid"))

  /** URL format */
  val textUrl = text.verifying(emptyOrRegexVerifying(
    """^(https?|ftp|file)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]""",
    "constraint.url",
    "error.invalid"
  ))
  /** Non-Empty URL format */
  val nonEmptyTextUrl = nonEmptyText.verifying(regexVerifying(
    """^(https?|ftp|file)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]""",
    "constraint.url",
    "error.invalid"
  ))

  @inline private def numberMapping[N: Numeric: Formatter](
    typeMin: N, typeMax: N, min: N, max: N, strict: Boolean): Mapping[N] = {
    val number = of[N]
    if (min == typeMin && max == typeMax) {
      number
    } else if (min == typeMin) {
      number verifying Constraints.max(max, strict)
    } else if (max == typeMax) {
      number verifying Constraints.min(min, strict)
    } else {
      number verifying (Constraints.min(min, strict), Constraints.max(max, strict))
    }
  }

  /**
   * Constructs a simple mapping for a numeric field (using a Float type behind).
   *
   * For example:
   * {{{
   * Form("size" -> floatNumber)
   * }}}
   */
  val floatNumber: Mapping[Float] = floatNumber()

  /**
   * Constructs a simple mapping for a numeric field (using a Double type behind).
   *
   * For example:
   * {{{
   * Form("size" -> doubleNumber)
   * }}}
   */
  val doubleNumber: Mapping[Double] = doubleNumber()

  /**
   * Constructs a simple mapping for a numeric field (using a BigInt type behind).
   *
   * For example:
   * {{{
   * Form("size" -> bigIntNumber)
   * }}}
   */
  val bigIntNumber: Mapping[BigInt] = of[BigInt]

  /**
   * Constructs a simple mapping for a numeric field (using a Float type behind).
   *
   * For example:
   * {{{
   * Form("size" -> floatNumber(min=0, max=100))
   * }}}
   *
   * @param min minimum value
   * @param max maximum value
   * @param strict should it be a strict comparison
   */
  def floatNumber(min: Float = Float.MinValue, max: Float = Float.MaxValue, strict: Boolean = false): Mapping[Float] =
    numberMapping[Float](Float.MinValue, Float.MaxValue, min, max, strict)

  /**
   * Constructs a simple mapping for a numeric field (using a Double type behind).
   *
   * For example:
   * {{{
   * Form("size" -> doubleNumber(min=0, max=100))
   * }}}
   *
   * @param min minimum value
   * @param max maximum value
   * @param strict should it be a strict comparison
   */
  def doubleNumber(min: Double = Double.MinValue, max: Double = Double.MaxValue, strict: Boolean = false): Mapping[Double] =
    numberMapping[Double](Double.MinValue, Double.MaxValue, min, max, strict)

  /**
   * Constructs a simple mapping for a numeric field (using a BigInt type behind).
   *
   * For example:
   * {{{
   * Form("size" -> bigIntNumber(min=0, max=100))
   * }}}
   *
   * @param min minimum value
   * @param max maximum value
   * @param strict should it be a strict comparison
   */
  def bigIntNumber(min: Option[BigInt], max: Option[BigInt], strict: Boolean = false): Mapping[BigInt] = (min, max) match {
    case (None, Some(a))    => bigIntNumber verifying (Constraints.max(a, strict))
    case (Some(i), None)    => bigIntNumber verifying (Constraints.min(i, strict))
    case (Some(i), Some(a)) => bigIntNumber verifying (Constraints.min(i, strict), Constraints.max(a, strict))
    case (None, None)       => bigIntNumber
  }

  /**
   * Constructs a simple mapping for a character field (using a Char type behind).
   *
   * For example:
   * {{{
   * Form("char" -> character)
   * }}}
   */
  val character: Mapping[Char] = of[Char]

}
