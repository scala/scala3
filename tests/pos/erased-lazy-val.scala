//> using options -language:experimental.erasedDefinitions

object Test {
  erased lazy val i: Int = 1 // now OK
}
