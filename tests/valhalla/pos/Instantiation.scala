import scala.annotation.valhalla

@valhalla
abstract class Foo[A](value: A) extends AnyVal with DeepValhalla

@valhalla
class Grault extends AnyVal with DeepValhalla

@valhalla
trait Corge[A] extends Any with DeepValhalla

@valhalla
class Baz[A](value: A) extends AnyVal with Corge[A]

class Qux

class Bar:
    def foobar() = {
        val x = new Foo(5) { }
        val w = new Foo(new Grault) { }
        val z = new Baz(new Grault)
    }

