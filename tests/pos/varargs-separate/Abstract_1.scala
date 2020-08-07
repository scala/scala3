import scala.annotation.varargs

abstract class Abs {
  @varargs def counter(s: String*) = ()
}

trait T {
  @varargs def counter(s: String*): Unit
}

