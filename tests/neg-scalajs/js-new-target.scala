import scala.scalajs.js
import scala.scalajs.js.annotation.*

object IllegalInScalaClass {
  class A {
    js.`new`.target // error: Illegal use of js.`new`.target.

    def this(x: Int) = {
      this()
      js.`new`.target // error: Illegal use of js.`new`.target.
    }
  }

  class B {
    def foo(x: Int): Unit =
      js.`new`.target // error: Illegal use of js.`new`.target.
  }

  class C extends js.Object {
    class D {
      js.`new`.target // error: Illegal use of js.`new`.target.
    }
  }
}

object IllegalInDefOrLazyVal {
  class A extends js.Object {
    lazy val x = js.`new`.target // error: Illegal use of js.`new`.target.
    def y: js.Dynamic = js.`new`.target // error: Illegal use of js.`new`.target.
    def z(x: Int): Any = js.`new`.target // error: Illegal use of js.`new`.target.
  }
}

object IllegalInLambdaOrByName {
  class A extends js.Object {
    val x = () => js.`new`.target // error: Illegal use of js.`new`.target.
    val y = Option(null).getOrElse(js.`new`.target) // error: Illegal use of js.`new`.target.
    val z: js.Function1[Int, Any] = (x: Int) => js.`new`.target // error: Illegal use of js.`new`.target.
    val w: js.ThisFunction0[Any, Any] = (x: Any) => js.`new`.target // error: Illegal use of js.`new`.target.
  }
}
