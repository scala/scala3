import annotation.experimental

class Class1:
  import language.experimental.namedTypeArguments // error
  import language.experimental.genericNumberLiterals // error
  import language.experimental.erasedDefinitions // ok: only check at erased definition

object Object1:
  import language.experimental.namedTypeArguments // error
  import language.experimental.genericNumberLiterals // error
  import language.experimental.erasedDefinitions // ok: only check at erased definition

def fun1 =
  import language.experimental.namedTypeArguments // error
  import language.experimental.genericNumberLiterals // error
  import language.experimental.erasedDefinitions // ok: only check at erased definition

val value1 =
  import language.experimental.namedTypeArguments // error
  import language.experimental.genericNumberLiterals // error
  import language.experimental.erasedDefinitions // ok: only check at erased definition
