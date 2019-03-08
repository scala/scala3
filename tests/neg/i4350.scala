import scala.quoted.Type

class Foo[T] {
  '{null.asInstanceOf[T]} // error
}
