import scala.annotation.valhalla

class Baz

@valhalla
class Grault[A](value: A) extends AnyVal with DeepValhalla

class Bar:
    def foobar() = {
        val u = new Grault(new Grault(new Baz)) // error
    }