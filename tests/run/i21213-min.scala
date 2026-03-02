import scala.language.experimental.modularity
import scala.language.future

sealed abstract class Foo(tracked val discriminator: String)
class Bar extends Foo("bar")

val bar: Foo = Bar()
object Test extends App:
  println(bar.discriminator)
