import caps.Pure
class P extends Pure
def foo[C <: Pure]() = ()
def bar[T, C <: Iterable[T] & Pure]() = ()
def baz[CC[_] <: Pure]() = ()
def bam[CC[A] <: Pure & Iterable[A]]() = ()
def test =
  foo[Int]()  // error
  bar[Int, List[Int]]() // OK
  baz[Seq]() // OK
  bam[Seq]() // OK
  foo[P]() // OK
