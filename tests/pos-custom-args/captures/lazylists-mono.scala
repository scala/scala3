class CC
type Cap = CC^

//-------------------------------------------------

def test(E: Cap) =

  trait LazyList[+A]:
    protected def contents: () ->{E} (A, LazyList[A]^{E})
    def isEmpty: Boolean
    def head: A = contents()._1
    def tail: LazyList[A]^{E} = contents()._2

  class LazyCons[+A](override val contents: () ->{E} (A, LazyList[A]^{E}))
  extends LazyList[A]:
    def isEmpty: Boolean = false

  object LazyNil extends LazyList[Nothing]:
    def contents: () ->{E} (Nothing, LazyList[Nothing]) = ???
    def isEmpty: Boolean = true

  extension [A](xs: LazyList[A]^{E})
    def map[B](f: A ->{E} B): LazyList[B]^{E} =
      if xs.isEmpty then LazyNil
      else
        val cons = () => (f(xs.head), xs.tail.map(f))
        LazyCons(cons)
