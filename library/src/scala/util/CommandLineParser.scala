package scala.util

import language.experimental.captureChecking

/** A utility object to support command line parsing for @main methods. */
object CommandLineParser {

  /** An exception raised for an illegal command line.
   *  @param idx  The index of the argument that's faulty (starting from 0)
   *  @param msg  The error message
   */
  class ParseError(val idx: Int, val msg: String) extends Exception

  /** Parses command line argument `s`, which has index `n`, as a value of type `T`.
   *
   *  @tparam T the target type to parse the string into
   *  @param str the command line argument string to parse
   *  @param n the zero-based index of the argument, used for error reporting
   *  @param fs the type class instance (usually provided implicitly) that converts a string to type `T`
   *  @throws ParseError if argument cannot be converted to type `T`.
   */
  def parseString[T](str: String, n: Int)(using fs: FromString[T]^): T = {
    try fs.fromString(str)
    catch {
      case ex: IllegalArgumentException => throw ParseError(n, ex.toString)
    }
  }

  /** Parses `n`'th argument in `args` (counting from 0) as a value of type `T`.
   *
   *  @tparam T the target type to parse the argument into
   *  @param args the command line arguments array
   *  @param n the zero-based index of the argument to parse
   *  @param fs the type class instance that converts a string to type `T`
   *  @throws ParseError if argument does not exist or cannot be converted to type `T`.
   */
  def parseArgument[T](args: Array[String], n: Int)(using fs: FromString[T]^): T =
    if n < args.length then parseString(args(n), n)
    else throw ParseError(n, "more arguments expected")

  /** Parses all arguments from `n`'th one (counting from 0) as a list of values of type `T`.
   *
   *  @tparam T the target type to parse each argument into
   *  @param args the command line arguments array
   *  @param n the zero-based index of the first remaining argument to parse
   *  @param fs the type class instance that converts a string to type `T`
   *  @throws ParseError if some of the arguments cannot be converted to type `T`.
   */
  def parseRemainingArguments[T](args: Array[String], n: Int)(using fs: FromString[T]^): List[T] =
    if n < args.length then parseString(args(n), n) :: parseRemainingArguments(args, n + 1)
    else Nil

  /** Prints error message explaining given ParserError.
   *
   *  @param err the parse error to display
   */
  def showError(err: ParseError): Unit = {
    val where =
      if err.idx == 0 then ""
      else if err.idx == 1 then " after first argument"
      else s" after ${err.idx} arguments"
    println(s"Illegal command line$where: ${err.msg}")
  }

  trait FromString[T] {
    /** Can throw java.lang.IllegalArgumentException.
     *
     *  @param s the string to convert to type `T`
     */
    def fromString(s: String): T

    def fromStringOption(s: String): Option[T] =
      try Some(fromString(s))
      catch {
        case ex: IllegalArgumentException => None
      }
  }

  object FromString {
    given FromString[String] with
      def fromString(s: String) = s

    given FromString[Boolean] with
      def fromString(s: String) = s.toBoolean

    given FromString[Byte] with
      def fromString(s: String) = s.toByte

    given FromString[Short] with
      def fromString(s: String) = s.toShort

    given FromString[Int] with
      def fromString(s: String) = s.toInt

    given FromString[Long] with
      def fromString(s: String) = s.toLong

    given FromString[Float] with
      def fromString(s: String) = s.toFloat

    given FromString[Double] with
      def fromString(s: String) = s.toDouble
  }
}
