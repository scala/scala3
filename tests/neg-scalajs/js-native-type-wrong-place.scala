import scala.scalajs.js
import scala.scalajs.js.annotation.*

// Nested in Scala class or trait

class A1 {
  @js.native // error
  trait X1 extends js.Object

  @js.native @JSGlobal("X") // error
  class X2 extends js.Object

  @js.native @JSGlobal("X") // error
  object X3 extends js.Object
}

trait A2 {
  @js.native // error
  trait X1 extends js.Object

  @js.native @JSGlobal("X") // error
  class X2 extends js.Object

  @js.native @JSGlobal("X") // error
  object X3 extends js.Object
}

// Nested in non-native JS type

class B1 extends js.Object {
  @js.native // error
  trait X1 extends js.Object

  @js.native @JSGlobal("X") // error
  class X2 extends js.Object

  @js.native @JSGlobal("X") // error
  object X3 extends js.Object
}

trait B2 extends js.Object {
  @js.native // error
  trait X1 extends js.Object

  @js.native @JSGlobal("X") // error
  class X2 extends js.Object

  @js.native @JSGlobal("X") // error
  object X3 extends js.Object
}

object B3 extends js.Object {
  @js.native // error
  trait X1 extends js.Object

  @js.native @JSGlobal("X") // error
  class X2 extends js.Object

  @js.native @JSGlobal("X") // error
  object X3 extends js.Object
}

// Local

object C {
  def a(): Unit = {
    @js.native @JSGlobal // error
    class X extends js.Object

    @js.native @JSGlobal // error
    object Y extends js.Object

    @js.native // error
    trait Z extends js.Object
  }
}
