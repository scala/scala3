package a

trait TC[E <: Expr]:
  type Elem = Expr.ExtractValue[E]
class BIExpr extends Expr:
  type Value = BigInt
class Foo extends TC[BIExpr]:
  val v: Elem = 0
