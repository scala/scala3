//> using options -experimental -Yno-experimental -Werror -WunstableInlineAccessors

import scala.quoted.*
import scala.annotation.publicInBinary

class Macro:
  inline def foo = ${ Macro.fooImpl }

object Macro:
  @publicInBinary private[Macro] def fooImpl(using Quotes) = '{}
