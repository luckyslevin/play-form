package ejisan.play.data

import scala.util.matching.Regex
import play.api.data.format.Formats._
import play.api.data.format.Formatter
import play.api.data.Forms._
import play.api.data.{ Mapping, OptionalMapping }
import play.api.data.validation._

import Formats._
import validation.Constraints

object Forms
  extends TextForms
  with NumberForms
  with HelperForms
  with JavaTimeForms
  with JodaTimeForms

trait HelperForms {

  /**
   * Constructs a mapping a field which mast be matched.
   *
   * For example:
   * {{{
   * Form("password" -> mapping(
   *   "try" -> nonEmptyText,
   *   "confirm" -> nonEmptyText
   * ))
   * }}}
   */
  final def matching[T](s1: (String, Mapping[T]), s2: (String, Mapping[T])): Mapping[T] = {
    tuple(s1, s2)
    .verifying(Constraint[(T, T)](Some("constraint.match"), Seq()){ case (s1: T, s2: T) =>
      if (s1 != s2) Invalid(ValidationError("error.match")) else Valid
    }).transform({ case (s1, _) => s1 }, (s1: T) => (s1, s1))
  }

  /**
   * Constructs a simple mapping for a numeric year field.
   *
   * For example:
   * {{{
   * Form("year" -> yearNumber)
   * }}}
   */
  final val yearNumber: Mapping[Int] = of[Int] verifying Constraints.year

  /**
   * Constructs a simple mapping for a numeric month field.
   *
   * For example:
   * {{{
   * Form("month" -> monthNumber)
   * }}}
   */
  final val monthNumber: Mapping[Int] = of[Int] verifying Constraints.month

  /**
   * Constructs a simple mapping for a numeric day field.
   *
   * For example:
   * {{{
   * Form("day" -> dayNumber)
   * }}}
   */
  final val dayNumber: Mapping[Int] = of[Int] verifying Constraints.day

  /**
   * Constructs a simple mapping for a numeric hour field.
   *
   * For example:
   * {{{
   * Form("hour" -> monthNumber)
   * }}}
   */
  final val hourNumber: Mapping[Int] = of[Int] verifying Constraints.hour

  /**
   * Constructs a simple mapping for a numeric minute field.
   *
   * For example:
   * {{{
   * Form("minute" -> minuteNumber)
   * }}}
   */
  final val minuteNumber: Mapping[Int] = of[Int] verifying Constraints.minute

  /**
   * Constructs a simple mapping for a numeric second field.
   *
   * For example:
   * {{{
   * Form("second" -> secondNumber)
   * }}}
   */
  final val secondNumber: Mapping[Int] = of[Int] verifying Constraints.second

  /**
   * Defines a mapping for selection. It constraints if it's not empty
   *
   * For example:
   * {{{
   * Form("country" -> selection(number))
   * }}}
   */
  final def selection[T](mapping: Mapping[T]): Mapping[T] = OptionalMapping(mapping).verifying(Constraints.some).transform(_.get, Option(_))


  /**
   * Defines a mapping for username.
   *
   * For example:
   * {{{
   * Form("username" -> usernameText)
   * }}}
   */
  final val usernameText: Mapping[String] = usernameText()

  /**
   * Defines a mapping for username.
   *
   * For example:
   * {{{
   * Form("username" -> usernameText(caseInsensitive = false, minLength = 8))
   * }}}
   */
  final def usernameText(caseInsensitive: Boolean = true, minLength: Int = 5, maxLength: Int = Int.MaxValue): Mapping[String]
    = text(minLength, maxLength).verifying(Constraints.username) transform (
      username => if (caseInsensitive) username.toUpperCase else username, username => username)


  /**
   * Defines a mapping for password.
   *
   * For example:
   * {{{
   * Form("password" -> passwordText)
   * }}}
   */
  final val passwordText: Mapping[String] = passwordText()

  /**
   * Defines a mapping for password.
   *
   * For example:
   * {{{
   * Form("password" -> passwordText(minLength = 8, hasUpperCase = true, hasSymbolical = false, hasDigit = true))
   * }}}
   */
  final def passwordText(minLength: Int = 8, maxLength: Int = Int.MaxValue, hasUpperCase: Boolean = true, hasSymbolical: Boolean = true, hasDigit: Boolean = true): Mapping[String]
    = text(minLength, maxLength) verifying Constraints.password(hasUpperCase, hasSymbolical, hasDigit, minLength)

}

trait JavaTimeForms extends HelperForms {
  import java.time.{ LocalDate, LocalDateTime, ZoneId }
  import java.time.format.DateTimeFormatter

  /**
   * Constructs a simple mapping for a date field (mapped as `java.time.LocalDate type`).
   *
   * For example:
   * {{{
   * Form("birthdate" -> localDate)
   * }}}
   */
  final val localDate: Mapping[LocalDate] = of[LocalDate]

  /**
   * Constructs a simple mapping for a date field (mapped as `java.time.LocalDate type`).
   *
   * For example:
   * {{{
   * import java.time.format.DateTimeFormatter
   * Form("birthdate" -> localDate(DateTimeFormatter.ISO_LOCAL_DATE))
   * }}}
   *
   * @param pattern the date pattern, as defined in `java.time.format.DateTimeFormatter`
   */
  // final def localDate(formatter: DateTimeFormatter): Mapping[LocalDate]
  //   = of[LocalDate] as localDateFormat(formatter)

  /**
   * Constructs a simple mapping for a date field (mapped as `java.time.LocalDate type`).
   *
   * For example:
   * {{{
   * Form("birthdate" -> localDate("dd-MM-yyyy"))
   * }}}
   *
   * @param pattern the date pattern, as defined in `java.time.format.DateTimeFormatter`
   */
  // final def localDate(pattern: String): Mapping[LocalDate]
  //   = of[LocalDate] as localDateFormat(DateTimeFormatter.ofPattern(pattern))

  /**
   * Constructs a separated parameter mapping for a date field (mapped as `java.time.LocalDate type`).
   *
   * For example:
   * {{{
   * Form("birthdate" -> separatedLocalDate())
   * }}}
   *
   * Or:
   * {{{
   * Form("birthdate" -> separatedLocalDate(
   *   "year" -> yearNumber,
   *   "month" -> monthNumber,
   *   "day" -> dayNumber
   * ))
   * }}}
   *
   * @param year mapping and key
   * @param month mapping and key
   * @param day mapping and key
   */
  final def separatedLocalDate(
    year: (String, Mapping[Int]) = ("year", yearNumber),
    month: (String, Mapping[Int]) = ("month", monthNumber),
    day: (String, Mapping[Int]) = ("day", dayNumber)
  ): Mapping[LocalDate] = tuple(
    (year._1, year._2 verifying Constraints.year),
    (month._1, month._2 verifying Constraints.month),
    (day._1, day._2 verifying Constraints.day)
  ) transform (
    { case (year, month, day) => LocalDate.of(year, month, day) },
    ld => { (ld.getYear, ld.getMonth.getValue, ld.getDayOfMonth) }
  )


  /**
   * Constructs a simple mapping for a date and time field (mapped as `java.time.LocalDate type`).
   *
   * For example:
   * {{{
   * Form("datetime" -> localDate)
   * }}}
   */
  final val localDateTime: Mapping[LocalDateTime] = of[LocalDateTime]

  /**
   * Constructs a simple mapping for a date and time field (mapped as `java.time.LocalDateTime type`).
   *
   * For example:
   * {{{
   * import java.time.format.DateTimeFormatter
   * Form("datetime" -> localDateTime(DateTimeFormatter.ISO_LOCAL_DATE_TIME))
   * }}}
   *
   * @param pattern the date and time pattern, as defined in `java.time.format.DateTimeFormatter`
   */
  // final def localDateTime(formatter: DateTimeFormatter): Mapping[LocalDateTime]
  //   = of[LocalDateTime] as localDateTimeFormat(formatter)

  /**
   * Constructs a simple mapping for a date and time field (mapped as `java.time.LocalDateTime type`).
   *
   * For example:
   * {{{
   * Form("datetime" -> localDateTime("yyyy-MM-dd HH:mm:ss"))
   * }}}
   *
   * @param pattern the date and time pattern, as defined in `java.time.format.DateTimeFormatter`
   */
  // final def localDateTime(pattern: String): Mapping[LocalDateTime]
  //   = of[LocalDateTime] as localDateTimeFormat(DateTimeFormatter.ofPattern(pattern))

  /**
   * Constructs a separated parameter mapping for a date and time field (mapped as `java.time.LocalDateTime type`).
   *
   * For example:
   * {{{
   * Form("datetime" -> separatedLocalDateTime())
   * }}}
   *
   * Or:
   * {{{
   * Form("datetime" -> separatedLocalDateTime(
   *   "year" -> yearNumber,
   *   "month" -> monthNumber,
   *   "day" -> dayNumber,
   *   "hour" -> hourNumber,
   *   "minute" -> minuteNumber,
   *   "second" -> secondNumber
   * ))
   * }}}
   *
   * @param year mapping and key
   * @param month mapping and key
   * @param day mapping and key
   * @param hour mapping and key
   * @param minute mapping and key
   * @param second mapping and key
   */
  final def separatedLocalDateTime(
    year: (String, Mapping[Int]) = ("year", yearNumber),
    month: (String, Mapping[Int]) = ("month", monthNumber),
    day: (String, Mapping[Int]) = ("day", dayNumber),
    hour: (String, Mapping[Int]) = ("hour", hourNumber),
    minute: (String, Mapping[Int]) = ("minute", minuteNumber),
    second: (String, Mapping[Int]) = ("second", secondNumber)
  ): Mapping[LocalDateTime] = tuple(
    (year._1, year._2 verifying Constraints.year),
    (month._1, month._2 verifying Constraints.month),
    (day._1, day._2 verifying Constraints.day),
    (hour._1, hour._2 verifying Constraints.hour),
    (minute._1, minute._2 verifying Constraints.minute),
    (second._1, second._2 verifying Constraints.second)
  ) transform (
    { case (year, month, day, hour, minute, second) => LocalDateTime.of(year, month, day, hour, minute, second) },
    ldt => { (ldt.getYear, ldt.getMonth.getValue, ldt.getDayOfMonth, ldt.getHour, ldt.getMinute, ldt.getSecond) }
  )


  /**
   * Constructs a simple mapping for a timezone field (mapped as `java.time.ZoneId type`).
   *
   * For example:
   * {{{
   * Form("zone" -> zoneId)
   * }}}
   */
  final val zoneId: Mapping[ZoneId] = of[ZoneId]
}

trait JodaTimeForms extends HelperForms {
  import java.time.{ LocalDateTime, LocalDate }

  /**
   * Constructs a separated parameter mapping for a date field (mapped as `org.joda.time.LocalDate type`).
   *
   * For example:
   * {{{
   * Form("birthdate" -> separatedJodaLocalDate())
   * }}}
   *
   * Or:
   * {{{
   * Form("birthdate" -> separatedJodaLocalDate(
   *   "year" -> yearNumber,
   *   "month" -> monthNumber,
   *   "day" -> dayNumber
   * ))
   * }}}
   *
   * @param year mapping and key
   * @param month mapping and key
   * @param day mapping and key
   */
  final def separatedJodaLocalDate(
    year: (String, Mapping[Int]) = ("year", yearNumber),
    month: (String, Mapping[Int]) = ("month", monthNumber),
    day: (String, Mapping[Int]) = ("day", dayNumber)
  ): Mapping[LocalDate] = tuple(
    (year._1, year._2 verifying Constraints.year),
    (month._1, month._2 verifying Constraints.month),
    (day._1, day._2 verifying Constraints.day)
  ) transform (
    { case (year, month, day) => LocalDate.of(year, month, day) },
    { (ld: LocalDate) => (ld.getYear, ld.getMonthValue, ld.getDayOfMonth) }
  )


  /**
   * Constructs a separated parameter mapping for a date and time field (mapped as `java.time.DateTime type`).
   *
   * For example:
   * {{{
   * Form("datetime" -> separatedDateTime())
   * }}}
   *
   * Or:
   * {{{
   * Form("datetime" -> separatedDateTime(
   *   "year" -> yearNumber,
   *   "month" -> monthNumber,
   *   "day" -> dayNumber,
   *   "hour" -> hourNumber,
   *   "minute" -> minuteNumber,
   *   "second" -> secondNumber
   * ))
   * }}}
   *
   * @param year mapping and key
   * @param month mapping and key
   * @param day mapping and key
   * @param hour mapping and key
   * @param minute mapping and key
   * @param second mapping and key
   */
  final def separatedDateTime(
    year: (String, Mapping[Int]) = ("year", yearNumber),
    month: (String, Mapping[Int]) = ("month", monthNumber),
    day: (String, Mapping[Int]) = ("day", dayNumber),
    hour: (String, Mapping[Int]) = ("hour", hourNumber),
    minute: (String, Mapping[Int]) = ("minute", minuteNumber),
    second: (String, Mapping[Int]) = ("second", secondNumber)
  ): Mapping[LocalDateTime] = tuple(
    (year._1, year._2 verifying Constraints.year),
    (month._1, month._2 verifying Constraints.month),
    (day._1, day._2 verifying Constraints.day),
    (hour._1, hour._2 verifying Constraints.hour),
    (minute._1, minute._2 verifying Constraints.minute),
    (second._1, second._2 verifying Constraints.second)
  ) transform (
    { case (year, month, day, hour, minute, second) => LocalDateTime.of(year, month, day, hour, minute, second) },
    (dt: LocalDateTime) => (dt.getYear, dt.getMonthValue, dt.getDayOfMonth, dt.getHour, dt.getMinute, dt.getSecond)
  )
}

trait TextForms {
  /**
   * Constructs a simple mapping for ASCII text field.
   *
   * Example:
   * {{{
   * Form("ascii" -> asciiText)
   * }}}
   */
  final val asciiText: Mapping[String] = asciiText()

  /**
   * Constructs a simple mapping for ASCII text field.
   *
   * Example:
   * {{{
   * Form("alphabets" -> asciiText(minLength=3))
   * }}}
   *
   * @param minLength Text min length.
   * @param maxLength Text max length.
   */
  final def asciiText(minLength: Int = 0, maxLength: Int = Int.MaxValue): Mapping[String]
    = text(minLength, maxLength) verifying Constraints.ascii

  /**
   * Constructs a simple mapping for required ASCII text field.
   *
   * Example:
   * {{{
   * Form("alphabets" -> nonEmptyAsciiText)
   * }}}
   */
  final val nonEmptyAsciiText: Mapping[String] = nonEmptyAsciiText()

  /**
   * Constructs a simple mapping for required ASCII text field.
   *
   * Example:
   * {{{
   * Form("alphabets" -> nonEmptyAsciiText(minLength=3))
   * }}}
   *
   * @param minLength Text min length.
   * @param maxLength Text max length.
   */
  final def nonEmptyAsciiText(minLength: Int = 0, maxLength: Int = Int.MaxValue): Mapping[String]
    = asciiText(minLength, maxLength) verifying Constraints.nonEmpty


  /**
   * Constructs a simple mapping for alphabetical text field.
   *
   * Example:
   * {{{
   * Form("alphabets" -> alphabeticalText)
   * }}}
   */
  final val alphabeticalText: Mapping[String] = alphabeticalText()

  /**
   * Constructs a simple mapping for alphabetical text field.
   *
   * Example:
   * {{{
   * Form("alphabets" -> alphabeticalText(minLength=3))
   * }}}
   *
   * @param minLength Text min length.
   * @param maxLength Text max length.
   */
  final def alphabeticalText(minLength: Int = 0, maxLength: Int = Int.MaxValue): Mapping[String]
    = text(minLength, maxLength) verifying Constraints.alphabetical

  /**
   * Constructs a simple mapping for required alphabetical text field.
   *
   * Example:
   * {{{
   * Form("alphabets" -> nonEmptyAlphabeticalText)
   * }}}
   */
  final val nonEmptyAlphabeticalText: Mapping[String] = nonEmptyAlphabeticalText()

  /**
   * Constructs a simple mapping for required alphabetical text field.
   *
   * Example:
   * {{{
   * Form("alphabets" -> nonEmptyAlphabeticalText(minLength=3))
   * }}}
   *
   * @param minLength Text min length.
   * @param maxLength Text max length.
   */
  final def nonEmptyAlphabeticalText(minLength: Int = 0, maxLength: Int = Int.MaxValue): Mapping[String]
    = alphabeticalText(minLength, maxLength) verifying Constraints.nonEmpty


  /**
   * Constructs a simple mapping for symbolical text field.
   *
   * Example:
   * {{{
   * Form("symbols" -> symbolicalText)
   * }}}
   */
  final val symbolicalText: Mapping[String] = symbolicalText()

  /**
   * Constructs a simple mapping for symbolical text field.
   *
   * Example:
   * {{{
   * Form("symbols" -> symbolicalText(minLength=3))
   * }}}
   *
   * @param minLength Text min length.
   * @param maxLength Text max length.
   */
  final def symbolicalText(minLength: Int = 0, maxLength: Int = Int.MaxValue): Mapping[String]
    = text(minLength, maxLength) verifying Constraints.symbolical

  /**
   * Constructs a simple mapping for required symbolical text field.
   *
   * Example:
   * {{{
   * Form("symbols" -> nonEmptySymbolicalText)
   * }}}
   */
  final val nonEmptySymbolicalText: Mapping[String] = nonEmptySymbolicalText()

  /**
   * Constructs a simple mapping for required symbolical text field.
   *
   * Example:
   * {{{
   * Form("symbols" -> nonEmptySymbolicalText(minLength=3))
   * }}}
   *
   * @param minLength Text min length.
   * @param maxLength Text max length.
   */
  final def nonEmptySymbolicalText(minLength: Int = 0, maxLength: Int = Int.MaxValue): Mapping[String]
    = symbolicalText(minLength, maxLength) verifying Constraints.nonEmpty


  /**
   * Constructs a simple mapping for a digit field. (A digit field allows to start with `0`)
   *
   * Example:
   * {{{
   * Form("phone" -> digit)
   * }}}
   */
  final val digit: Mapping[String] = digit()

  /**
   * Constructs a simple mapping for a digit field. (A digit field allows to start with `0`)
   *
   * Example:
   * {{{
   * Form("phone" -> digit(minLength=10, maxLength=11))
   * }}}
   *
   * @param minLength Text min length.
   * @param maxLength Text max length.
   */
  final def digit(minLength: Int = 0, maxLength: Int = Int.MaxValue): Mapping[String]
    = text(minLength, maxLength) verifying Constraints.digit

  /**
   * Constructs a simple mapping for required digit field. (A digit field allows to start with `0`)
   *
   * Example:
   * {{{
   * Form("phone" -> nonEmptyDigit)
   * }}}
   */
  final val nonEmptyDigit: Mapping[String] = nonEmptyDigit()

  /**
   * Constructs a simple mapping for required digit field. (A digit field allows to start with `0`)
   *
   * Example:
   * {{{
   * Form("phone" -> nonEmptyDigit(minLength=10, maxLength=11))
   * }}}
   *
   * @param minLength Text min length.
   * @param maxLength Text max length.
   */
  final def nonEmptyDigit(minLength: Int = 0, maxLength: Int = Int.MaxValue): Mapping[String]
    = digit(minLength, maxLength) verifying Constraints.nonEmpty


  /**
   * Constructs a simple mapping for URL text field.
   *
   * Example:
   * {{{
   * Form("website" -> url)
   * }}}
   */
  final val url: Mapping[String] = url()

  /**
   * Constructs a simple mapping for URL text field.
   *
   * Example:
   * {{{
   * Form("website" -> url(protocols="https?|http", minLength=5, maxLength=255))
   * }}}
   *
   * @param minLength Text min length.
   * @param maxLength Text max length.
   */
  final def url(protocols: String = "https?|http|ftp|ftps|file", minLength: Int = 0, maxLength: Int = Int.MaxValue): Mapping[String]
    = text(minLength, maxLength) verifying Constraints.url(protocols)

  /**
   * Constructs a simple mapping for required URL text field.
   *
   * Example:
   * {{{
   * Form("website" -> nonEmptyUrl)
   * }}}
   */
  final val nonEmptyUrl: Mapping[String] = nonEmptyUrl()

  /**
   * Constructs a simple mapping for required URL text field.
   *
   * Example:
   * {{{
   * Form("website" -> digit(protocols="https?|http", minLength=5, maxLength=255))
   * }}}
   *
   * @param minLength Text min length.
   * @param maxLength Text max length.
   */
  final def nonEmptyUrl(protocols: String = "https?|http|ftp|ftps|file", minLength: Int = 0, maxLength: Int = Int.MaxValue): Mapping[String]
    = url(protocols, minLength, maxLength) verifying Constraints.nonEmpty
}

trait NumberForms {
  @inline private[this] final def numberMapping[N: Numeric: Formatter](
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
   * Constructs a simple mapping for `Double` typed numeric field (using a Double type behind).
   *
   * For example:
   * {{{
   * Form("price" -> doubleNumber)
   * }}}
   */
  final val doubleNumber: Mapping[Double] = doubleNumber()

  /**
   * Constructs a simple mapping for `Double` typed numeric field.
   *
   * For example:
   * {{{
   * Form("price" -> doubleNumber(min=0.0d, max=1000000.0d))
   * }}}
   *
   * @param min minimum value
   * @param max maximum value
   * @param strict should it be a strict comparison
   */
  final def doubleNumber(min: Double = Double.MinValue, max: Double = Double.MaxValue, strict: Boolean = false): Mapping[Double]
  = numberMapping[Double](Double.MinValue, Double.MaxValue, min, max, strict)


  /**
   * Constructs a simple mapping for `Float` typed numeric field (using a Float type behind).
   *
   * For example:
   * {{{
   * Form("latitude" -> floatNumber)
   * }}}
   */
  final val floatNumber: Mapping[Float] = floatNumber()

  /**
   * Constructs a simple mapping for `Float` typed numeric field.
   *
   * For example:
   * {{{
   * Form("latitude" -> floatNumber(min=0.0f, max=90.99f))
   * }}}
   *
   * @param min minimum value
   * @param max maximum value
   * @param strict should it be a strict comparison
   */
  final def floatNumber(min: Float = Float.MinValue, max: Float = Float.MaxValue, strict: Boolean = false): Mapping[Float] =
    numberMapping[Float](Float.MinValue, Float.MaxValue, min, max, strict)
}

trait JapaneseSupportForms {
  /**
   * Constructs a simple mapping for zenkaku(全角) text field.
   *
   * Example:
   * {{{
   * Form("zenkaku" -> zenkakuText)
   * }}}
   */
  final val zenkakuText: Mapping[String] = zenkakuText()

  /**
   * Constructs a simple mapping for zenkaku(全角) text field.
   *
   * Example:
   * {{{
   * Form("zenkaku" -> zenkakuText(minLength=3, maxLength=10))
   * }}}
   */
  final def zenkakuText(minLength: Int = 0, maxLength: Int = Int.MaxValue): Mapping[String]
    = text(minLength, maxLength) verifying Constraints.zenkaku

  /**
   * Constructs a simple mapping for required zenkaku(全角) text field.
   *
   * Example:
   * {{{
   * Form("zenkaku" -> nonEmptyZenkakuText)
   * }}}
   */
  final val nonEmptyZenkakuText: Mapping[String] = nonEmptyZenkakuText()

  /**
   * Constructs a simple mapping for required zenkaku(全角) text field.
   *
   * Example:
   * {{{
   * Form("zenkaku" -> nonEmptyZenkakuText(minLength=3, maxLength=10))
   * }}}
   */
  final def nonEmptyZenkakuText(minLength: Int = 0, maxLength: Int = Int.MaxValue): Mapping[String]
    = zenkakuText(minLength, maxLength) verifying Constraints.nonEmpty


  /**
   * Constructs a simple mapping for hiragana(ひらがな) text field.
   *
   * Example:
   * {{{
   * Form("hiragana" -> hiraganaText)
   * }}}
   */
  final val hiraganaText: Mapping[String] = hiraganaText()

  /**
   * Constructs a simple mapping for hiragana(ひらがな) text field.
   *
   * Example:
   * {{{
   * Form("hiragana" -> hiraganaText(minLength=3, maxLength=10))
   * }}}
   */
  final def hiraganaText(minLength: Int = 0, maxLength: Int = Int.MaxValue): Mapping[String]
    = text(minLength, maxLength) verifying Constraints.hiragana

  /**
   * Constructs a simple mapping for required hiragana(ひらがな) text field.
   *
   * Example:
   * {{{
   * Form("hiragana" -> nonEmptyHiraganaText)
   * }}}
   */
  final val nonEmptyHiraganaText: Mapping[String] = nonEmptyHiraganaText()

  /**
   * Constructs a simple mapping for required hiragana(ひらがな) text field.
   *
   * Example:
   * {{{
   * Form("hiragana" -> nonEmptyHiraganaText(minLength=3, maxLength=10))
   * }}}
   */
  final def nonEmptyHiraganaText(minLength: Int = 0, maxLength: Int = Int.MaxValue): Mapping[String]
    = hiraganaText(minLength, maxLength) verifying Constraints.nonEmpty


  /**
   * Constructs a simple mapping for katakana(カタカナ) text field.
   *
   * Example:
   * {{{
   * Form("katakana" -> katakanaText)
   * }}}
   */
  final val katakanaText: Mapping[String] = katakanaText()

  /**
   * Constructs a simple mapping for katakana(カタカナ) text field.
   *
   * Example:
   * {{{
   * Form("katakana" -> katakanaText(minLength=3, maxLength=10))
   * }}}
   */
  final def katakanaText(minLength: Int = 0, maxLength: Int = Int.MaxValue): Mapping[String]
    = text(minLength, maxLength) verifying Constraints.katakana

  /**
   * Constructs a simple mapping for required katakana(カタカナ) text field.
   *
   * Example:
   * {{{
   * Form("katakana" -> nonEmptyKatakanaText)
   * }}}
   */
  final val nonEmptyKatakanaText: Mapping[String] = nonEmptyKatakanaText()

  /**
   * Constructs a simple mapping for required katakana(カタカナ) text field.
   *
   * Example:
   * {{{
   * Form("katakana" -> nonEmptyKatakanaText(minLength=3, maxLength=10))
   * }}}
   */
  final def nonEmptyKatakanaText(minLength: Int = 0, maxLength: Int = Int.MaxValue): Mapping[String]
    = katakanaText(minLength, maxLength) verifying Constraints.nonEmpty
}
