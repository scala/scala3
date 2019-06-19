package dotty.internal

object StringContextMacro {

  @forceInline def f(sc: => scala.StringContext)(args: Any*): String =
    throw new Exception("non-boostrapped library")

}
