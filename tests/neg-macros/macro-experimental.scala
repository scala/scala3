

import scala.quoted.*
import scala.annotation.experimental

inline def f = ${ impl } // error
@experimental def impl(using Quotes) = '{1}
