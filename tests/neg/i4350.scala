import scala.quoted.TypeTag

class Foo[T] {
  '{null.asInstanceOf[T]} // error
}
