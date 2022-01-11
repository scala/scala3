import annotation.experimental

@experimental
object Object1:
  import language.experimental.fewerBraces
  import language.experimental.namedTypeArguments
  import language.experimental.genericNumberLiterals
  import language.experimental.erasedDefinitions
  erased def f: Int

object Object2:
  import language.experimental.fewerBraces // error
  import language.experimental.namedTypeArguments // error
  import language.experimental.genericNumberLiterals // error
  import language.experimental.erasedDefinitions
  erased def f: Int

@experimental
object Class1:
  import language.experimental.fewerBraces
  import language.experimental.namedTypeArguments
  import language.experimental.genericNumberLiterals
  import language.experimental.erasedDefinitions
  erased def f: Int

object Class2:
  import language.experimental.fewerBraces // error
  import language.experimental.namedTypeArguments // error
  import language.experimental.genericNumberLiterals // error
  import language.experimental.erasedDefinitions
  erased def f: Int

@experimental
def fun1 =
  import language.experimental.fewerBraces
  import language.experimental.namedTypeArguments
  import language.experimental.genericNumberLiterals
  import language.experimental.erasedDefinitions
  erased def f: Int

def fun2 =
  import language.experimental.fewerBraces // error
  import language.experimental.namedTypeArguments // error
  import language.experimental.genericNumberLiterals // error
  import language.experimental.erasedDefinitions
  erased def f: Int
