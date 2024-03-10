//> using options -experimental -Yno-experimental

@newUnusedSymbol
class Foo

@newUnusedSymbol
object Foo

@main def Test(): Unit =
  val foo = new Foo
