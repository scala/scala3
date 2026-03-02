import scala.language.experimental.modularity
import scala.language.future

enum Foo(tracked val discriminator: String):
  case Bar() extends Foo("bar")
  case Baz() extends Foo("baz")

val bar: Foo = Foo.Bar()
object Test extends App:
  println(bar.discriminator)
