import scala.quoted.*

class Macro:
  inline def foo = ${ Macro.fooImpl }

object Macro:
  private def fooImpl(using Quotes) = '{}
