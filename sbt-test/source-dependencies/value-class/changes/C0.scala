import scala.language.unsafeNulls

object C extends App {
  println(new B().foo(null))
}
