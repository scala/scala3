import scala.annotation.valhalla

class Baz

@valhalla
abstract class ValueOption[+A] extends AnyVal with DeepValhalla

@valhalla
class ValueSome[+A](value: A) extends ValueOption[A] {
    def get: A = value
}

/*  

A DeepValhalla class must not have a non-DeepValhalla field 

*/

@valhalla
class Fred(f: ValueOption[Baz]) extends AnyVal with DeepValhalla // error

@valhalla
class Garply(value: Option[Int]) extends AnyVal with DeepValhalla // error