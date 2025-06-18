import language.experimental.captureChecking
import caps.*

package test1:

  trait Collection[T] extends Mutable:
    update def add(elem: T): Unit
    update def remove(elem: T): Unit
    def get(index: Int): Option[T]

  object Collection:
    def empty[T]: Collection[T] = ???

  trait Foo:
    val thunks: Collection[() => Unit] // that's fine

  object FooImpl1 extends Foo:
    val thunks: Collection[() => Unit] = Collection.empty // error
    val thunks2: Collection[() => Unit] = Collection.empty[() => Unit] // error
    val thunks3: Collection[() => Unit] = Collection.empty[() => Unit] // error

package test2:

  trait Collection[+T] extends Mutable:
    def get(index: Int): Option[T]

  object Collection:
    def empty[T]: Collection[T] = ???

  trait Foo:
    val thunks: Collection[() => Unit] // that's fine

  object FooImpl1 extends Foo:
    val thunks: Collection[() => Unit] = Collection.empty // error
    val thunks2: Collection[() => Unit] = Collection.empty[() => Unit] // error
    val thunks3: Collection[() => Unit] = Collection.empty[() => Unit] // error