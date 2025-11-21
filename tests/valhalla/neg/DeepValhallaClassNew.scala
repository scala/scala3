import scala.annotation.valhalla

class Baz

@valhalla
class Grault[A](value: A) extends AnyVal with DeepValhalla

class Bar:
    def foobar() = {
        val v = new Grault(new Baz) // error
    }