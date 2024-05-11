object test:

  class FileSystem extends caps.Capability

  def foo(fs: FileSystem) =

    trait LazyList[+A]:
      this: LazyList[A]^{fs} =>

      def isEmpty: Boolean
      def head: A
      def tail: LazyList[A]^{this}

    object LazyNil extends LazyList[Nothing]:
      def isEmpty: Boolean = true
      def head = ???
      def tail = ???

    final class LazyCons[+T](val x: T, val xs: () => LazyList[T]^) extends LazyList[T]: // error
      def isEmpty = false
      def head = x
      def tail: LazyList[T]^{this} = xs()
    end LazyCons

    new LazyCons(1, () => LazyNil)

