import scala.annotation.valhalla

class Baz

@valhalla
class Grault[A](value: A) extends AnyVal with DeepValhalla

class Bar:
    def foobar() = {
        val w = new Grault(new Some(new Baz)) // error
    }