import caps.Pure
def foo[C <: Pure]() = ()
def bar[T, C <: Iterable[T] & Pure]() = ()
def baz[CC[_] <: Pure]() = ()
def bam[CC[A] <: Pure & Iterable[A]]() = ()
def test =
  foo[Int]()
  bar[Int, List[Int]]()
  baz[Seq]()
  bam[Seq]()
