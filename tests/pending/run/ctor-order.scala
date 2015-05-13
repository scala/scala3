
/** Test that constructor operations are reordered correctly.  */
class Outer {

  object global {
    val x = 10;
  }

  class X extends AnyRef with M1 {
// TODO NEEDS MANUAL CHANGE (early initializers)
// BEGIN copied early initializers
val outer = Outer.this
// END copied early initializers
}

  trait M1 { self: X =>
    Console.println(global.x);
    Console.println(outer.global.x);
  }

}

object Test extends AnyRef with App {
  val o = new Outer;

  new o.X;
}
