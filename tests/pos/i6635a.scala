object Test {
  abstract class ExprBase { s =>
    type A
  }

  abstract class Lit extends ExprBase { s =>
    type A = Int
    val n: A
  }

  // Fails recheck since the result type e2.A is converted to Int to avoid
  // a false dependency on e2.
  def castTestFail2a(e1: ExprBase)(e2: Lit with e1.type)(x: e1.A): e2.A = x
}
