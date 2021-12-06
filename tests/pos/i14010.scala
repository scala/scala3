abstract class LazyList[+T] {
  def head: T
  def tail: LazyList[T]
  def isEmpty: Boolean
  def push[E >: T](top: => E): LazyList[E] =
    new Push[E](top, this)
  //def map[R](f: T => R): LazyList[R]
  def append[E >: T](that: => LazyList[E]): LazyList[E]
}

private class Push[+T](top: => T, stack: => LazyList[T]) extends LazyList[T] {
  override def head: T =
    top
  override def tail: LazyList[T] =
    stack
  override def isEmpty: Boolean =
    false
  //override def map[R](f: T => R): LazyList[R] =
  //  new Push[R](f(top), stack.map(f)) {
  //        override def map[R2](f2: R => R2): LazyList[R2] =
  //          Push.this.map(f2 compose f)
  //      }
  override def append[E >: T](that: => LazyList[E]): LazyList[E] =
    new Push[E](top, stack.append(that)) {
          override def append[E2 >: E](that2: => LazyList[E2]): LazyList[E2] =
            Push.this.append(that.append(that2))
        }
}

object LazyList {
  val empty =
    new LazyList[Nothing] {
          override def head: Nothing =
            throw new NoSuchElementException
          override def tail: LazyList[Nothing] =
            throw new UnsupportedOperationException
          override def isEmpty: Boolean =
            true
          //override def map[R](f: _ => R): LazyList[R] =
          //  this
          override def append[E](that: => LazyList[E]): LazyList[E] =
            that
        }
  def apply[T](elements: T*): LazyList[T] =
    elements.foldRight[LazyList[T]](empty)(new Push(_, _))
}