object Test {
  abstract class ExprBase { s =>
    type A
  }

  abstract class Lit extends ExprBase { s =>
    type A = Int
    val n: A
  }

  abstract class LitU extends ExprBase { s =>
    type A <: Int
    val n: A
  }

  abstract class LitL extends ExprBase { s =>
    type A <: Int
    val n: A
  }

  def castTest1(e1: ExprBase)(e2: e1.type)(x: e1.A): e2.A = x
  def castTest2(e1: ExprBase { type A = Int })(e2: e1.type)(x: e1.A): e2.A = x
  def castTest3(e1: ExprBase)(e2: ExprBase with e1.type)(x: e2.A): e1.A = x

  def castTest4(e1: ExprBase { type A = Int })(e2: ExprBase with e1.type)(x: e2.A): e1.A = x

  def castTest5a(e1: ExprBase)(e2: LitU with e1.type)(x: e2.A): e1.A = x
  def castTest5b(e1: ExprBase)(e2: LitL with e1.type)(x: e2.A): e1.A = x

  //fail:
  def castTestFail1(e1: ExprBase)(e2: Lit with e1.type)(x: e2.A): e1.A = x // this is like castTest5a/b, but with Lit instead of LitU/LitL
  // the other direction never works:
  def castTestFail2a(e1: ExprBase)(e2: Lit with e1.type)(x: e1.A): e2.A = x
  def castTestFail2b(e1: ExprBase)(e2: LitL with e1.type)(x: e1.A): e2.A = x
  def castTestFail2c(e1: ExprBase)(e2: LitU with e1.type)(x: e1.A): e2.A = x

  // the problem isn't about order of intersections.
  def castTestFail2bFlip(e1: ExprBase)(e2: e1.type with LitL)(x: e1.A): e2.A = x
  def castTestFail2cFlip(e1: ExprBase)(e2: e1.type with LitU)(x: e1.A): e2.A = x

  def castTestFail3(e1: ExprBase)(e2: Lit with e1.type)(x: e1.A): e2.A = {
    val y: e1.type with e2.type = e2
    val z = x: y.A
    z
  }
}