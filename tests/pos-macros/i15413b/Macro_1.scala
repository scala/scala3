import scala.quoted.*
import scala.annotation.inlineAccessible

inline def foo = ${ fooImpl }

@inlineAccessible private def fooImpl(using Quotes) = '{}
