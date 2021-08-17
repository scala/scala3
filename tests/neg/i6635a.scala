object Test {
  abstract class ExprBase { s =>
    type A
  }

  abstract class Lit extends ExprBase { s =>
    type A = Int
    val n: A
  }

  // It would be nice if the following could typecheck. We'd need to apply
  // a reasoning like this:
  //
  //    Since there is an argument `e2` of type `Lit & e1.type`, it follows that
  //    e1.type == e2.type Hence, e1.A == e2.A == Int. This looks similar
  //    to techniques used in GADTs.
  //
  def castTestFail2a(e1: ExprBase)(e2: Lit & e1.type)(x: e1.A): Int = x // error: Found: (x : e1.A) Required: Int
}
