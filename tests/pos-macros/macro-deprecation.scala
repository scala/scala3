import scala.quoted.*

inline def f = ${ impl } // error
@deprecated def impl(using Quotes) = '{1}
