//> using options -Werror -WunstableInlineAccessors

import scala.language.experimental.mode
import scala.quoted.*
import scala.annotation.publicInBinary

class Macro:
  inline def foo = ${ Macro.fooImpl }

object Macro:
  @publicInBinary private[Macro] def fooImpl(using Quotes) = '{}
