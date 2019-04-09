trait Foo[State] {
  def bar(f: Bar[State] => Bar[State]): Foo[_] = this
}
object Foo {
  def unit: Foo[_] = new Foo[Any] {}
  def doBar: Foo[_] = unit.bar(bar => bar)
}
class Bar[+A]
