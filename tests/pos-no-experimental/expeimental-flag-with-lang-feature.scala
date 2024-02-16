//> using options -experimental -Yno-experimental

import scala.language.experimental.erasedDefinitions
import scala.language.experimental.namedTypeArguments

erased def erasedFun(erased x: Int): Int = x

def namedTypeArgumentsFun[T, U]: Int =
  namedTypeArgumentsFun[T = Int, U = Int]
  namedTypeArgumentsFun[U = Int, T = Int]
