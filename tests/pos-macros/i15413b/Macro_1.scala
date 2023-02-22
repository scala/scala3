package bar

import scala.quoted.*
import scala.annotation.binaryAPI

inline def foo = ${ fooImpl }

@binaryAPI private[bar] def fooImpl(using Quotes) = '{}
