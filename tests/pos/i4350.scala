import scala.quoted.Type

class Foo[T: Type] {
  '{null.asInstanceOf[T]}
}
