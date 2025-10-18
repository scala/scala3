class LazyListIterable[+A]

private sealed trait State[+A]:
  def head: A
  def tail: LazyListIterable[A]^

private object State:
  object Empty extends State[Nothing]:
    def head: Nothing = throw new NoSuchElementException("head of empty lazy list")
    def tail: LazyListIterable[Nothing] = throw new UnsupportedOperationException("tail of empty lazy list")

  final class Cons[A](val head: A, val tail: LazyListIterable[A]^) extends State[A] // error

