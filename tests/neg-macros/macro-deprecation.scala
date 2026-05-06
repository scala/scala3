//> using options -Werror -deprecation

import scala.quoted.*

inline def f = ${ impl } // warn (in .check file)
@deprecated def impl(using Quotes) = '{1}

// nopos-error No warnings can be incurred under -Werror (or -Xfatal-warnings)
