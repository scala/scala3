trait X {
  type Y
  abstract val v: Y // error: abstract term
  abstract def y: Y // error: abstract term
}

implicit object Z { // error: implict at toplevel
  implicit case class C() // error: implicit classes may not be case classes
  implicit type T = Int // error: implicit modifier cannot be used for types or traits
  implicit trait U // error: implicit modifier cannot be used for types or traits
  val x: X = new X {
    type Y = Int
    val v: Int = 1
  }
  var y: Int // error: only classes can have declared but undefined members
  val z: Int = {
    val u: Int // error: only classes can have declared but undefined members
    1
  }
}

trait T {
  type X
  def foo: Unit = {
    var x: Int // error: only classes can have declared but undefined members
    ()
  }
  private def bar: Int // error: abstract member may not have private modifier
  final def baz: Int // error: abstract member may not have final modifier
}

final sealed class A { // error: illegal combination of modifiers: final and sealed
  private protected def f: Int = 1 // error: illegal combination of modifiers: private and protected
}


class E extends T {
  abstract override def foo: Unit // error: abstract override only allowed for members of traits
}

trait U extends T {
  abstract override type X // error: `abstract override' incompatible with type definition
  @native def f(): Unit = 1 // error: `@native' members may not have implementation
}

trait TT extends AnyVal // error: trait TT annot extend AnyVal

final trait UU  // error: trait UU may not be `final'
