package ejisan.play.data.validation

import play.api.data.validation._

object Constraints extends play.api.data.validation.Constraints {

  /**
   * Defines a‘required’ constraint for `Char` values, i.e. one in which empty char is invalid.
   *
   * '''name'''[constraint.required]
   * '''error'''[error.required]
   */
  def nonEmptyChar: Constraint[Char] = Constraint[Char]("constraint.required") { o =>
    if (o == null) Invalid(ValidationError("error.required")) else Valid
  }

  /**
   * Defines a‘required’ constraint for `Option[T]` values.
   *
   * '''name'''[constraint.required]
   * '''error'''[error.required]
   */
  def some[T]: Constraint[Option[T]] = Constraint[Option[T]]("constraint.required") { o =>
    if (o == null) Invalid(ValidationError("error.required")) else if (o.isEmpty) Invalid(ValidationError("error.required")) else Valid
  }

  /**
   * Defines a `year` constraint for `Int` typed year values.
   *
   * '''name'''[constraint.year]
   * '''error'''[error.year]
   */
  def year: Constraint[Int] = Constraint[Int]("constraint.year") { o =>
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
  def month: Constraint[Int] = Constraint[Int]("constraint.month") { o =>
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
  def day: Constraint[Int] = Constraint[Int]("constraint.day") { o =>
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
  def hour: Constraint[Int] = Constraint[Int]("constraint.hour") { o =>
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
  def minute: Constraint[Int] = Constraint[Int]("constraint.minute") { o =>
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
  def second: Constraint[Int] = Constraint[Int]("constraint.second") { o =>
    if (o == null) Invalid(ValidationError("error.second")) else try {
      val second = o.toInt
      if (second >= 0 && second < 60) Valid
      else Invalid(ValidationError("error.second"))
    } catch {
      case _: Throwable => Invalid(ValidationError("error.second"))
    }
  }


  val alphabetical = pattern(
    """^[a-zA-Z]+$""".r,
    "constraint.alphabetical",
    "error.alphabetical")
  val symbolical = pattern(
    """^[a-zA-Z]+$""".r,
    "constraint.symbolical",
    "error.symbolical")
  val digit = pattern(
    """^[0-9]+$""".r,
    "constraint.digit",
    "error.digit")
  def url(protocol: String = "https?|http|ftp|ftps|file") = pattern(
    """^(https?|ftp|file)://[-a-zA-Z0-9+&@#/%?=~_|!:,.;]*[-a-zA-Z0-9+&@#/%=~_|]""".r,
    "constraint.url",
    "error.url")
  val httpUrl = url("http")
  val httpsUrl = url("https")
  val ftpUrl = url("ftp")
  val ftpsUrl = url("ftps")
  val fileUrl = url("file")

}
