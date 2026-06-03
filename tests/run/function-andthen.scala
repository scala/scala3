// Directional tests for `andThen` on `Function2..Function22` and `scala.runtime.FunctionXXL`.
// Mirrors the behavior of `java.util.function.BiFunction.andThen`.
object Test {

  def main(args: Array[String]): Unit = {
    // Reusable post-composition function.
    val toStr: Any => String = _.toString

    // 1. Function2.andThen — the common case (mirrors `java.util.function.BiFunction.andThen`).
    val add: (Int, Int) => Int = _ + _
    val addThenStr = add.andThen(toStr)
    assert(addThenStr(1, 2) == "3")
    // Result is a Function2 (not a Function1), so we can keep applying it as such.
    val addThenStr2: (Int, Int) => String = add.andThen(toStr)
    assert(addThenStr2(10, 20) == "30")
    // Identity composition: `f.andThen(identity) == f` semantically.
    val id: Int => Int = x => x
    assert(add.andThen(id)(3, 4) == add(3, 4))

    // 2. Function3 — sanity check at arity 3.
    val sum3: (Int, Int, Int) => Int = _ + _ + _
    assert(sum3.andThen(toStr)(1, 2, 3) == "6")

    // 3. Function22 — exercise the largest hand-written arity.
    val sum22: (Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int,
                Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int) => Int =
      (a1, a2, a3, a4, a5, a6, a7, a8, a9, a10, a11,
       a12, a13, a14, a15, a16, a17, a18, a19, a20, a21, a22) =>
        a1 + a2 + a3 + a4 + a5 + a6 + a7 + a8 + a9 + a10 + a11 +
        a12 + a13 + a14 + a15 + a16 + a17 + a18 + a19 + a20 + a21 + a22
    val sum22Str = sum22.andThen(toStr)
    assert(sum22Str(1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1,
                    1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1) == "22")

    // 4. Type inference — chained andThen with type-changing functions.
    val mul: (Int, Int) => Int = _ * _
    val pipeline: (Int, Int) => Int = mul.andThen(_ + 1).andThen(_ * 2)
    assert(pipeline(3, 4) == (3 * 4 + 1) * 2)

    // 5. Side-effects are evaluated in `apply`-then-`g` order.
    val sb = new StringBuilder
    val tap2: (Int, Int) => Int = (x, y) => { sb.append("apply;"); x + y }
    val after: Any => Unit = _ => sb.append("g;")
    tap2.andThen(after).apply(1, 2)
    assert(sb.toString == "apply;g;", s"unexpected order: ${sb.toString}")

    // 6. `andThen` returns a fresh function — no aliasing with `this`.
    assert((add.andThen(id): AnyRef) ne (add: AnyRef))

    // 7. Plays nice with `tupled` / `curried`.
    val addTupled: ((Int, Int)) => Int = add.tupled
    val tupledThenStr = addTupled.andThen(toStr)
    assert(tupledThenStr((5, 6)) == "11")
    val curriedThenStr = add.curried.andThen(_.andThen(toStr))
    assert(curriedThenStr(5)(6) == "11")

    // 8. FunctionXXL — runtime helper for arity > 22; type info is erased to Object.
    val xxl: scala.runtime.FunctionXXL = new scala.runtime.FunctionXXL {
      def apply(xs: IArray[Object]): Object =
        Integer.valueOf(xs.foldLeft(0)((acc, o) => acc + o.asInstanceOf[Integer].intValue))
    }
    val xxlThen: scala.runtime.FunctionXXL =
      xxl.andThen(o => ("v=" + o).asInstanceOf[Object])
    val args23: IArray[Object] = IArray.tabulate(23)(i => Integer.valueOf(i + 1).asInstanceOf[Object])
    assert(xxlThen.apply(args23) == "v=276") // 1+2+...+23 == 276
  }
}
