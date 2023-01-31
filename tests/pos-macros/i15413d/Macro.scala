import scala.quoted.*

inline def foo = ${ fooImpl }

private def fooImpl(using Quotes) = '{}
