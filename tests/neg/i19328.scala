import scala.language.implicitConversions

object i19328:

  trait Foo[B]
  given foo[C]: Foo[C] = new Foo[C] {}

  type Id[A] = A

  implicit def wrapId[A](a: A): Id[A] = a

  def bar(using bool: Boolean): Unit = ()

  bar // error: missing implicit (should not crash)
