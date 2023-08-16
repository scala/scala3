import scala.quoted.*

class Macro:
  inline def foo = ${ Macro.fooImpl } // error

object Macro:
  private def fooImpl(using Quotes) = '{}
