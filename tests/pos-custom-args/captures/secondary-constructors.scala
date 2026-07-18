package scala
package collection
package immutable
import caps.unsafe.untrackedCaptures

final class LazyListIterable[+A]():
  this: LazyListIterable[A]^ =>
  @untrackedCaptures var _head: Any = 0
  private def this(head: A, tail: LazyListIterable[A]^) =
    this()
    _head = head

object LazyListIterable:
  @inline private def eagerCons[A](hd: A, tl: LazyListIterable[A]^): LazyListIterable[A]^{tl} = new LazyListIterable[A](hd, tl)