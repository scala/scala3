// scalac: -language:experimental.erasedDefinitions

object Test {
  erased var i: Int = 1 // error
}
