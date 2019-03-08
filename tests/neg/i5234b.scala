final class Not2[T] private ()

trait LowPriorityNot2 {

  /** A fallback method used to emulate negation in Scala 2 */
  implicit def default[T]: Not2[T] = Not2.value.asInstanceOf[Not2[T]]
}
object Not2 extends LowPriorityNot2 {

  /** A value of type `Not` to signal a successful search for `Not[C]` (i.e. a failing
   *  search for `C`). A reference to this value will be explicitly constructed by
   *  Dotty's implicit search algorithm
   */
  def value: Not2[Nothing] = new Not2[Nothing]()

  /** One of two ambiguous methods used to emulate negation in Scala 2 */
  implicit def amb1[T](implicit ev: T): Not2[T] = ???

  /** One of two ambiguous methods used to emulate negation in Scala 2 */
  implicit def amb2[T](implicit ev: T): Not2[T] = ???
}

object Test {
  class Foo
  class Bar
  implicit def foo: Foo = ???
  implicitly[Foo]
  implicitly[Not2[Foo]] // error
  implicitly[Not2[Bar]]
}
