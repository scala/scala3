import scala.scalajs.js
import scala.scalajs.js.annotation.*

    object Enclosing1 {
      class A extends js.Object {
        private[Enclosing1] def foo(i: Int): Int = i // error
        private[Enclosing1] val x: Int = 3 // error
        private[Enclosing1] var y: Int = 5 // error

        final private[Enclosing1] def babar(i: Int): Int = i // ok
      }

      class B extends A {
        override private[Enclosing1] final def foo(i: Int): Int = i + 1
      }
    }

    object Enclosing2 {
      object A extends js.Object {
        private[Enclosing2] def foo(i: Int): Int = i // error
        private[Enclosing2] val x: Int = 3 // error
        private[Enclosing2] var y: Int = 5 // error

        final private[Enclosing2] def babar(i: Int): Int = i // ok
      }
    }

    object Enclosing3 {
      abstract class A extends js.Object {
        private[Enclosing3] def foo(i: Int): Int // error
        private[Enclosing3] val x: Int // error
        private[Enclosing3] var y: Int // error

        final private[Enclosing3] def babar(i: Int): Int = i // ok
      }

      class B extends A {
        override private[Enclosing3] final def foo(i: Int): Int = i + 1
      }
    }

    object Enclosing4 {
      trait A extends js.Object {
        private[Enclosing4] def foo(i: Int): Int // error
        private[Enclosing4] val x: Int // error
        private[Enclosing4] var y: Int // error
      }
    }

    object Enclosing5 {
      class A private () extends js.Object // ok
      class B private[this] () extends js.Object // ok
      class C private[Enclosing5] () extends js.Object // ok
    }
