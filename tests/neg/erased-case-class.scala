//> using options -experimental -language:experimental.erasedDefinitions

case class Foo1(erased x: Int) // error // error
