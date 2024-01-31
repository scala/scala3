//> using options -Xfatal-warnings

trait Monad[T]:
  def id: String
class Foo
object Foo {
  given Monad[Foo] with { def id = "Foo" }
}

opaque type Bar = Foo
object Bar {
  given Monad[Bar] = summon[Monad[Foo]] // warn
}

object Test extends App {
  println(summon[Monad[Foo]].id)
  println(summon[Monad[Bar]].id)
}
// nopos-error: No warnings can be incurred under -Werror (or -Xfatal-warnings)