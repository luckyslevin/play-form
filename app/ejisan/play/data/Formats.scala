package ejisan.play.data

import play.api.data._
import play.api.data.validation._
import play.api.data.Forms._
import play.api.data.format.{Formats => PlayFormats}
import play.api.data.format.Formatter
import play.api.data.format.Formats.stringFormat

object Formats {

  /**
   * Helper for formatters binders
   * @param parse Function parsing a String value into a T value, throwing an exception in case of failure
   * @param error Error to set in case of parsing failure
   * @param key Key name of the field to parse
   * @param data Field data
   */
  private def parsing[T](parse: String => T, errMsg: String, errArgs: Seq[Any])(key: String, data: Map[String, String]): Either[Seq[FormError], T] = {
    PlayFormats.stringFormat.bind(key, data).right.flatMap { s =>
      scala.util.control.Exception.allCatch[T]
        .either(parse(s))
        .left.map(e => Seq(FormError(key, errMsg, errArgs)))
    }
  }

  private def numberFormatter[T](convert: String => T, real: Boolean = false): Formatter[T] = {
    val (formatString, errorString) = if (real) ("format.real", "error.real") else ("format.numeric", "error.number")
    new Formatter[T] {
      override val format = Some(formatString -> Nil)
      def bind(key: String, data: Map[String, String]) =
        parsing(convert, errorString, Nil)(key, data)
      def unbind(key: String, value: T) = Map(key -> value.toString)
    }
  }

  /**
   * Default formatter for the `BigInt` type.
   */
  implicit def bigIntFormat: Formatter[BigInt] = numberFormatter(BigInt(_))

  /**
   * Default formatter for the `Char` type.
   */
  implicit def charFormat: Formatter[Char] = {
    val (formatString, errorString) = ("format.char", "error.char")
    new Formatter[Char] {
      override val format = Some(formatString -> Nil)
      def bind(key: String, data: Map[String, String]) =
        parsing({ str =>
          if (str.length == 1) str.toCharArray()(0)
          else throw new Exception("Length of string for `Char` type must be one")
        }, errorString, Nil)(key, data)
      def unbind(key: String, value: Char) = Map(key -> value.toString)
    }
  }

}
