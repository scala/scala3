//> using options -experimental -language:experimental.erasedDefinitions

object Foo {
  type X = (using ) => Int // error: an identifier expected, but ')' found
  def ff: X = () // error: found: Unit, expected: Int

  type Y = (erased) => Int // error: empty function may not be erased
  def gg: Y = () // error: found: Unit, expected: Y

  type Z = (erased given) => Int // error: empty function may not be erased
  def hh: Z = () // error: found: Unit, expected: Int
}
