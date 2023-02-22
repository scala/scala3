import scala.quoted.*
import scala.annotation.binaryAPI

class Macro:
  inline def foo = ${ Macro.fooImpl }

object Macro:
  @binaryAPI private[Macro] def fooImpl(using Quotes) = '{}
