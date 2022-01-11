import language.experimental.erasedDefinitions

type Ev = { type Out }
type Sub[X] = Ev { type Out = X }

object test1:
  transparent inline def foo(ev: Ev): Option[ev.Out] = ???
  transparent inline def bar[X](ev: Sub[X]): Option[ev.Out] = ???
  def test =
    foo(???)
    val y = bar[Int](???)
    y: Option[Int]

object test2:
  inline def foo(ev: Ev): Option[ev.Out] = ???
  inline def bar[X](ev: Sub[X]): Option[ev.Out] = ???
  def test =
    foo(???)
    val y = bar[Int](???)
    y: Option[Int]

object test3:
  inline def bar(ev: Ev)(x: ev.Out): Option[ev.Out] = Some(x)
  val a: Ev { type Out = Int } = ???
  def test =
    val y = bar(a)(???)
    y: Option[Int]

object test4:
  inline def bar(ev: Ev, x: ev.Out): Option[ev.Out] = Some(x)
  val a: Ev { type Out = Int } = ???
  def test =
    val y = bar(a, ???)
    y: Option[Int]

final class CallbackTo[+A] {
  inline def map[B](f: A => B)(using erased ev: CallbackTo.MapGuard[B]): CallbackTo[ev.Out] = ???
}

object CallbackTo {

  type MapGuard[A] = { type Out = A }
  erased given MapGuard[A]: MapGuard[A]

  def traverse[A, B](ta: List[A]): CallbackTo[List[B]] =
    val x: CallbackTo[List[A] => List[B]] = ???
    x.map(_(ta))
}

