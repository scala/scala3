// Directional tests for `scala.Function.identity`.
object Test {
  def main(args: Array[String]): Unit = {
    // 1. Returns its input argument unchanged.
    assert(Function.identity[Int].apply(42) == 42)
    assert(Function.identity[String].apply("hello") eq "hello")
    assert(Function.identity[AnyRef].apply(null) == null)

    // 2. Singleton sharing: distinct type instantiations resolve to the same
    //    underlying function instance (no per-call allocation).
    val idInt: Int => Int = Function.identity
    val idStr: String => String = Function.identity
    val idObj: Object => Object = Function.identity
    assert(idInt.asInstanceOf[AnyRef] eq idStr.asInstanceOf[AnyRef])
    assert(idInt.asInstanceOf[AnyRef] eq idObj.asInstanceOf[AnyRef])
    assert(idInt.asInstanceOf[AnyRef] eq Function.identity[Int].asInstanceOf[AnyRef])

    // 3. Suitable as a function argument to higher-order combinators.
    val xs = List(1, 2, 3)
    assert(xs.map(Function.identity) == xs)
    val nested = List(List(1), List(2, 3))
    assert(nested.flatMap(Function.identity) == List(1, 2, 3))

    // 4. Behaves consistently with `Predef.identity` for arbitrary inputs.
    val sample: List[Any] = List(1, "x", null, (1, 2), Nil)
    assert(sample.map(Function.identity) == sample.map(scala.Predef.identity))

    // 5. Composes with `andThen` / `compose` like any other Function1.
    val plusOne: Int => Int = _ + 1
    assert((Function.identity[Int] andThen plusOne)(10) == 11)
    assert((plusOne compose Function.identity[Int])(10) == 11)
  }
}
