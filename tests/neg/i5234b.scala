final class NotGiven2[T] private ()

trait LowPriorityNotGiven2 {

  /** A fallback method used to emulate negation in Scala 2 */
  implicit def default[T]: NotGiven2[T] = NotGiven2.value.asInstanceOf[NotGiven2[T]]
}
object NotGiven2 extends LowPriorityNotGiven2 {

  /** A value of type `NotGiven` to signal a successful search for `NotGiven[C]` (i.e. a failing
   *  search for `C`). A reference to this value will be explicitly constructed by
   *  Dotty's implicit search algorithm
   */
  def value: NotGiven2[Nothing] = new NotGiven2[Nothing]()

  /** One of two ambiguous methods used to emulate negation in Scala 2 */
  implicit def amb1[T](implicit ev: T): NotGiven2[T] = ???

  /** One of two ambiguous methods used to emulate negation in Scala 2 */
  implicit def amb2[T](implicit ev: T): NotGiven2[T] = ???
}

object Test {
  class Foo
  class Bar
  implicit def foo: Foo = ???
  implicitly[Foo]
  implicitly[NotGiven2[Foo]] // error
  implicitly[NotGiven2[Bar]]
}
