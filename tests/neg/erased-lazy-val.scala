//> using options -experimental -language:experimental.erasedDefinitions

object Test {
  erased lazy val i: Int = 1 // error
}
