trait Foo {
  type State
  def bar(f: Bar[State] => Bar[State]): Foo = this
}
object Foo {
  def unit: Foo = new Foo {
    type State = Any
  }
  def doBar: Foo = unit.bar(bar => bar)
}
class Bar[+A]
