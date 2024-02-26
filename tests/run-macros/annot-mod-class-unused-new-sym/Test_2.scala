import scala.language.experimental.mode
@newUnusedSymbol
class Foo

@newUnusedSymbol
object Foo

@main def Test(): Unit =
  val foo = new Foo
