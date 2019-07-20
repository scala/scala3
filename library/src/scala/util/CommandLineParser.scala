package scala.util

object CommandLineParser {

  /** An exception raised for an illegal command line
    *  @param idx  The index of the argument that's faulty (starting from 0)
    *  @param msg  The error message
    */
  class ParseError(val idx: Int, val msg: String) extends Exception

  /** Parse command line argument `s`, which has index `n`, as a value of type `T` */
  def parseString[T](str: String, n: Int) given (fs: FromString[T]): T = {
    try fs.fromString(str)
    catch {
      case ex: IllegalArgumentException => throw ParseError(n, ex.toString)
    }
  }

  /** Parse `n`'th argument in `args` (counting from 0) as a value of type `T` */
  def parseArgument[T](args: Array[String], n: Int) given (fs: FromString[T]): T =
    if n < args.length then parseString(args(n), n)
    else throw ParseError(n, "more arguments expected")

  /** Parse all arguments from `n`'th one (counting from 0) as a list of values of type `T` */
  def parseRemainingArguments[T](args: Array[String], n: Int) given (fs: FromString[T]): List[T] =
    if n < args.length then parseString(args(n), n) :: parseRemainingArguments(args, n + 1)
    else Nil

  /** Print error message explaining given ParserError */
  def showError(err: ParseError): Unit = {
    val where =
      if err.idx == 0 then ""
      else if err.idx == 1 then " after first argument"
      else s" after ${err.idx} arguments"
    println(s"Illegal command line$where: ${err.msg}")
  }
}

/* A function like

      @main f(x: S, ys: T*) = ...

   would be translated to something like

      import CommandLineParser._
      class f {
        @static def main(args: Array[String]): Unit =
          try
            f(
              parseArgument[S](args, 0),
              parseRemainingArguments[T](args, 1): _*
            )
          catch case err: ParseError => showError(err)
        }
*/