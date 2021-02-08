package example

import scala.language.implicitConversions

class ImplicitConversion {
  import ImplicitConversion.*
  implicit def string2Number(
      string: String
  ): Int = 42
  val message = ""
  val number = 42
  val tuple = (1, 2)
  val char: Char = 'a'

  // extension methods
  message
    .stripSuffix("h")
  tuple + "Hello"

  // implicit conversions
  val x: Int = message

  // interpolators
  s"Hello $message $number"
  s"""Hello
     |$message
     |$number""".stripMargin

  val a: Int = char
  val b: Long = char
}

object ImplicitConversion {
  implicit final class newAny2stringadd[A](private val self: A) extends AnyVal {
    def +(other: String): String = String.valueOf(self) + other
  }
}
