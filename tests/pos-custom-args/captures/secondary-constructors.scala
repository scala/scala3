package scala
package collection
package immutable

final class LazyListIterable[+A]():
  this: LazyListIterable[A]^ =>
  var _head: Any = 0
  private def this(head: A, tail: LazyListIterable[A]^) =
    this()
    _head = head

object LazyListIterable:
  @inline private def eagerCons[A](hd: A, tl: LazyListIterable[A]^): LazyListIterable[A]^{tl} = new LazyListIterable[A](hd, tl)