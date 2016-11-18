import scala.annotation._

@binaryCompatible
class Foo {
}

@binaryCompatible
case class CC(a: Int) { // error: synthesized methods
}

@binaryCompatible
class Repeated(a: Int*) { // error: desugared repeated
}

@binaryCompatible
trait Interface { // error: gets an init
 val s = 1        // error: not abstract and field
}

@binaryCompatible
class HasField {
 val s = 1
}
