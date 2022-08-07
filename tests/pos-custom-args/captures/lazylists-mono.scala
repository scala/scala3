class CC
type Cap = {*} CC

//-------------------------------------------------

def test(E: Cap) =

  trait LazyList[+A]:
    protected def contents: {E} () -> (A, {E} LazyList[A])
    def isEmpty: Boolean
    def head: A = contents()._1
    def tail: {E} LazyList[A] = contents()._2

  class LazyCons[+A](override val contents: {E} () -> (A, {E} LazyList[A]))
  extends LazyList[A]:
    def isEmpty: Boolean = false

  object LazyNil extends LazyList[Nothing]:
    def contents: {E} () -> (Nothing, LazyList[Nothing]) = ???
    def isEmpty: Boolean = true

  extension [A](xs: {E} LazyList[A])
    def map[B](f: {E} A -> B): {E} LazyList[B] =
      if xs.isEmpty then LazyNil
      else
        val cons = () => (f(xs.head), xs.tail.map(f))
        new LazyCons(cons.asInstanceOf) // !!!
