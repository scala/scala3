//> using options -Xfatal-warnings

import scala.language.experimental.avoidLoopingGivens

trait Monad[T]:
  def id: String
class Foo
object Foo {
  given Monad[Foo] with { def id = "Foo" }
}

opaque type Bar = Foo
object Bar {
  given Monad[Bar] = summon[Monad[Foo]] // was error fixed by avoidLoopingGivens
}

object Test extends App {
  println(summon[Monad[Foo]].id)
  println(summon[Monad[Bar]].id)
}