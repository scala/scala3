//> using options -experimental -Werror -WunstableInlineAccessors

package bar

import scala.quoted.*
import scala.annotation.publicInBinary

inline def foo = ${ fooImpl }

@publicInBinary private[bar] def fooImpl(using Quotes) = '{}
