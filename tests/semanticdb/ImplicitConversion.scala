package example

import scala.language.implicitConversions

class ImplicitConversion {
  implicit def string2Number(
      string: String
  ): Int = 42
  implicit def newAny2StringAdd[T](
      any: T
  ): Predef.any2stringadd[T] = new Predef.any2stringadd(any)
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
