enum Outer:
  case Foo
object Outer:
  trait Bar
  case class Baz(bar: Bar)
  case class Bam(bar: Bar = new Bar() {})
