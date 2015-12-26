case class C[T](x: T)

case class CS(xs: C[_]*)

// t3856
object Test {
  val x = CS(C(5), C("abc")) match { case CS(C(5), xs : _*) => xs }
    // Invalid: Vararg pattern cannot be split between normal and :_* patterns.
    // This split also does not work for vararg arguments, so there's no
    // good argument it should work for patterns
  println(x)

  def foo(xs: Int*) = ()
  val xs = List(1, 2, 3)
  foo(1, xs:_*)
}
