/* It is tempting to relax the `isConcrete` test of match types for `AndType`
 * in the following way:
 *
 * (isConcrete(tp.tp1) || !tp.tp1.derivesFrom(targetClass)) && (... same for tp.tp2)
 *
 * but the test in this file shows that this would be unsound.
 *
 * If we did relax the rule, it would help usages of match types applied to the
 * singleton type of term pattern match scrutinees. For example:
 *
 * def foo[VS <: Tuple](x: VS): SelectH[VS] = x match
 *   case x: (h *: t) => x.head
 *
 * The type of `x` in the branch body is `(VS & (h *: t))`. The result of
 * `x.head` is therefore a `Tuple.Head[VS & (h *: t)]`, which does not reduce
 * according to the current rules, but would reduce to `h` with the relaxed
 * rule.
 *
 * Note that the code can be fixed with an explicit type argument to `.head`:
 *
 * def foo[VS <: Tuple](x: VS): SelectH[VS] = x match
 *   case x: (h *: t) => x.head[h *: t]
 *
 * So it *seems* like it would be fine to relax the rule, based on the insight
 * that `VS` in `Tuple.Head[VS & (h *: t)]` does not contribute anything to the
 * computed type capture in `Tuple.Head`.
 *
 * The test is this file demonstrates that relaxing the rule can cause
 * unsoundness. So don't do it.
 */

type SelectH[VS <: Tuple] = VS match
  case h *: ? => h

// The original example found in the fingo/spata library
object ExampleFromSpata:
  def foo[VS <: Tuple](x: VS): SelectH[VS] = x match
    case x: (h *: t) => x.head // error

  def bar[VS <: Tuple](x: VS): SelectH[VS] = x match
    case x: (h *: t) => x.head[h *: t] // ok
end ExampleFromSpata

trait Z {
  type Y <: Tuple
  type W <: Tuple
  def unpair: SelectH[Y & W]
}

class A extends Z {
  type Y = Tuple2[Any, Any]
  def unpair: SelectH[Y & W] = "" // error
  def any: Any = unpair
}

class B extends A { this: Z =>
  type W = Tuple2[Int, Int]
  def int: Int = unpair
}

class C extends A { this: Z =>
  type W = Tuple2[String, String]
  def string: String = unpair
}

object Main {
  def main(args: Array[String]): Unit =
    println((new B).int + 1) // would give ClassCastException
}
