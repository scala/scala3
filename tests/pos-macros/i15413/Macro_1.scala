import scala.quoted.*
import scala.annotation.inlineAccessible

class Macro:
  inline def foo = ${ Macro.fooImpl }

object Macro:
  @inlineAccessible private def fooImpl(using Quotes) = '{}
