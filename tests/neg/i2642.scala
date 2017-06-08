object Foo {
  type X = implicit () => Int // error: implicit function needs parameters
  def ff: X = () // error: found: Unit, expected: X
}
