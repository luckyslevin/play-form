package ejisan.play.data.validation

import scala.util.matching.Regex
import play.api.data.validation._

object Constraints
  extends play.api.data.validation.Constraints
  with HelperConstraints
  with DateTimeConstraints
  with TextPatternConstraints
  with JapaneseConstraints

trait HelperConstraints {
  /**
   * Defines a‘required’ constraint for `Char` values, i.e. one in which empty char is invalid.
   *
   * '''name'''[constraint.required]
   * '''error'''[error.required]
   */
  final val nonEmptyChar: Constraint[Char] = Constraint[Char]("constraint.required") { o =>
    if (o == null) Invalid(ValidationError("error.required"))
    else Valid
  }


  /**
   * Defines a‘required’ constraint for `Option[T]` values.
   *
   * '''name'''[constraint.required]
   * '''error'''[error.required]
   */
  final def some[T]: Constraint[Option[T]] = Constraint[Option[T]]("constraint.required") { o =>
    if (o == null) Invalid(ValidationError("error.required"))
    else if (o.isEmpty) Invalid(ValidationError("error.required"))
    else Valid
  }


  /**
   * Defines a `username` constraint for `String` typed username values.
   *
   * '''name'''[constraint.username]
   * '''error'''[error.username]
   */
  final val username: Constraint[String] = username()

  /**
   * Defines a `username` constraint for `String` typed username values.
   *
   * '''name'''[constraint.username]
   * '''error'''[error.username]
   *
   * @param regex for username.
   */
  final def username(regex: Regex = """^[a-zA-Z0-9][a-zA-Z0-9\.!#$%&'*+/=?^_`{|}~-]*$""".r): Constraint[String]
    = Constraints.pattern(regex, "constraint.username", "error.username")


  /**
   * Defines a `password` constraint for `String` typed password values.
   *
   * '''name'''[constraint.password]
   * '''error'''[error.password]
   */
  final val password: Constraint[String] = password()

  /**
   * Defines a `password` constraint for `String` typed password values.
   *
   * '''name'''[constraint.password]
   * '''error'''[error.password]
   *
   * @param hasUpperCase  A password has upper cased text or not.
   * @param hasSymbolical A password has symbolical text or not.
   * @param hasDigit      A password has digit or not.
   * @param minLength     Minimum length for a password.
   */
  final def password(
    hasUpperCase: Boolean = true, hasSymbolical: Boolean = true, hasDigit: Boolean = true, minLength: Int = 8): Constraint[String] = {
    val upperCase = if (hasUpperCase) """(?=.*[A-Z])""" else ""
    val symbolical = if (hasSymbolical) """(?=.*[ -/:-@\[-\`\{-\~])""" else ""
    val digit = if (hasDigit) """(?=.*[0-9])""" else ""
    Constraints.pattern(
      s"""^.*(?=.{$minLength,})(?=.*[a-z])${upperCase}${symbolical}${digit}(?=.*).*$$""".r,
      "constraint.password",
      "error.password")
  }
}

trait DateTimeConstraints {
  /**
   * Defines a `year` constraint for `Int` typed year values.
   *
   * '''name'''[constraint.year]
   * '''error'''[error.year]
   */
  final val year: Constraint[Int] = Constraint[Int]("constraint.year") { o =>
    if (o == null) Invalid(ValidationError("error.year")) else try {
      val year = o.toInt
      if (year > 0 && year < 10000) Valid
      else Invalid(ValidationError("error.year"))
    } catch {
      case _: Throwable => Invalid(ValidationError("error.year"))
    }
  }

  /**
   * Defines a `month` constraint for `Int` typed month values.
   *
   * '''name'''[constraint.month]
   * '''error'''[error.month]
   */
  final val month: Constraint[Int] = Constraint[Int]("constraint.month") { o =>
    if (o == null) Invalid(ValidationError("error.month")) else try {
      val month = o.toInt
      if (month > 0 && month <= 12) Valid
      else Invalid(ValidationError("error.month"))
    } catch {
      case _: Throwable => Invalid(ValidationError("error.month"))
    }
  }

  /**
   * Defines a `day` constraint for `Int` typed day values.
   *
   * '''name'''[constraint.day]
   * '''error'''[error.day]
   */
  final val day: Constraint[Int] = Constraint[Int]("constraint.day") { o =>
    if (o == null) Invalid(ValidationError("error.day")) else try {
      val day = o.toInt
      if (day > 0 && day <= 31) Valid
      else Invalid(ValidationError("error.day"))
    } catch {
      case _: Throwable => Invalid(ValidationError("error.day"))
    }
  }

  /**
   * Defines a `hour` constraint for `Int` typed hour values.
   *
   * '''name'''[constraint.hour]
   * '''error'''[error.hour]
   */
  final val hour: Constraint[Int] = Constraint[Int]("constraint.hour") { o =>
    if (o == null) Invalid(ValidationError("error.hour")) else try {
      val hour = o.toInt
      if (hour >= 0 && hour < 24) Valid
      else Invalid(ValidationError("error.hour"))
    } catch {
      case _: Throwable => Invalid(ValidationError("error.hour"))
    }
  }

  /**
   * Defines a `minute` constraint for `Int` typed minute values.
   *
   * '''name'''[constraint.minute]
   * '''error'''[error.minute]
   */
  final val minute: Constraint[Int] = Constraint[Int]("constraint.minute") { o =>
    if (o == null) Invalid(ValidationError("error.minute")) else try {
      val minute = o.toInt
      if (minute >= 0 && minute < 60) Valid
      else Invalid(ValidationError("error.minute"))
    } catch {
      case _: Throwable => Invalid(ValidationError("error.minute"))
    }
  }

  /**
   * Defines a `second` constraint for `Int` typed second values.
   *
   * '''name'''[constraint.second]
   * '''error'''[error.second]
   */
  final val second: Constraint[Int] = Constraint[Int]("constraint.second") { o =>
    if (o == null) Invalid(ValidationError("error.second")) else try {
      val second = o.toInt
      if (second >= 0 && second < 60) Valid
      else Invalid(ValidationError("error.second"))
    } catch {
      case _: Throwable => Invalid(ValidationError("error.second"))
    }
  }
}

trait TextPatternConstraints {
  /**
   * Defines a `ASCII` constraint for `String` typed ASCII values.
   *
   * '''name'''[constraint.ascii]
   * '''error'''[error.ascii]
   */
  final val ascii: Constraint[String]
    = Constraints.pattern("""^[\\u0000-\\u007F]*$""".r, "constraint.ascii", "error.ascii")

  /**
   * Defines a `alphabetical` constraint for `String` typed alphabetical values.
   *
   * '''name'''[constraint.alphabetical]
   * '''error'''[error.alphabetical]
   */
  final val alphabetical: Constraint[String]
    = Constraints.pattern("""^[a-zA-Z ]*$""".r, "constraint.alphabetical", "error.alphabetical")

  /**
   * Defines a `symbolical` constraint for `String` typed symbolical values.
   *
   * '''name'''[constraint.symbolical]
   * '''error'''[error.symbolical]
   */
  final val symbolical: Constraint[String]
    = Constraints.pattern("""^[ -/:-@\[-\`\{-\~]*$""".r, "constraint.symbolical", "error.symbolical")

  /**
   * Defines a `digit` constraint for `String` typed digit values.
   *
   * '''name'''[constraint.digit]
   * '''error'''[error.digit]
   */
  final val digit: Constraint[String]
    = Constraints.pattern("""^[0-9]*$""".r, "constraint.digit", "error.digit")

  /**
   * Defines a `url` constraint for `String` typed url values.
   *
   * '''name'''[constraint.url]
   * '''error'''[error.url]
   */
  final def url(protocols: String = "https?|http|ftp|ftps|file"): Constraint[String] = Constraints.pattern(
    s"""^($protocols)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]""".r,
    "constraint.url",
    "error.url")
}

trait JapaneseConstraints {
  /**
   * Defines a `zenkaku` constraint for `String` typed zenkaku(全角) values.
   *
   * '''name'''[constraint.zenkaku]
   * '''error'''[error.zenkaku]
   */
  final val zenkaku: Constraint[String]
    = Constraints.pattern("""^[^ -~｡-ﾟ]*$""".r, "constraint.zenkaku", "error.zenkaku")

  /**
   * Defines a `hiragana` constraint for `String` typed hiragana(ひらがな) values.
   *
   * '''name'''[constraint.hiragana]
   * '''error'''[error.hiragana]
   */
  final val hiragana: Constraint[String]
    = Constraints.pattern("""^[　|\s|ぁ-ゟ|゠|ー]*$""".r, "constraint.hiragana", "error.hiragana")

  /**
   * Defines a `katakana` constraint for `String` typed katakana(カタカナ) values.
   *
   * '''name'''[constraint.katakana]
   * '''error'''[error.katakana]
   */
  final val katakana: Constraint[String]
    = Constraints.pattern("""^[　|\s|゠-ヿ]*$""".r, "constraint.katakana", "error.katakana")
}
