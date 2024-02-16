//> using options -Yno-experimental

import scala.language.experimental.erasedDefinitions

erased def erasedFun(erased x: Int): Int = x // error // error
