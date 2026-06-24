import scala.annotation.valhalla

class Baz

@valhalla
abstract class ValueOption[+A] extends AnyVal with DeepValhalla

@valhalla
case class ValueSome[+A](value: A) extends ValueOption[A] {
    def get: A = value
}

class Bar:
    def foobar() = {
        val z = ValueSome(new Baz) // error
    }