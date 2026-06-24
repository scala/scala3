import scala.annotation.valhalla

class Baz

@valhalla
abstract class Foo[A](value: A) extends AnyVal with DeepValhalla

class Bar:
    def foobar() = {
        val y = new Foo(new Some(new Baz)) {} // error
    }