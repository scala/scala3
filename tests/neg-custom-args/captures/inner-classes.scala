object test:

  @annotation.capability class FileSystem

  def foo(fs: FileSystem) =

    trait LazyList[+A]:
      this: {fs} LazyList[A] =>

      def isEmpty: Boolean
      def head: A
      def tail: {this} LazyList[A]

    object LazyNil extends LazyList[Nothing]:
      def isEmpty: Boolean = true
      def head = ???
      def tail = ???

    final class LazyCons[+T](val x: T, val xs: () => {*} LazyList[T]) extends LazyList[T]: // error
      def isEmpty = false
      def head = x
      def tail: {this} LazyList[T] = xs()
    end LazyCons

    new LazyCons(1, () => LazyNil)

