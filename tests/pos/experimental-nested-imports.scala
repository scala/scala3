//> using options -Yno-experimental

import annotation.experimental

class Class1:
  import language.experimental.namedTypeArguments
  import language.experimental.genericNumberLiterals
  import language.experimental.erasedDefinitions
  @experimental def f = 1

object Object1:
  import language.experimental.namedTypeArguments
  import language.experimental.genericNumberLiterals
  import language.experimental.erasedDefinitions
  @experimental def f = 1

def fun1 =
  import language.experimental.namedTypeArguments
  import language.experimental.genericNumberLiterals
  import language.experimental.erasedDefinitions
  @experimental def f = 1

val value1 =
  import language.experimental.namedTypeArguments
  import language.experimental.genericNumberLiterals
  import language.experimental.erasedDefinitions
  @experimental def f = 1
