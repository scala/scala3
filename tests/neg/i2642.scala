//> using options -language:experimental.erasedDefinitions

object Foo {
  type X = (using ) => Int // error
  def ff: X = () // error

  type Y = (erased) => Int // error
  def gg: Y = () // error

  type Z = (erased given) => Int // error // error
  def hh: Z = () // error
}
