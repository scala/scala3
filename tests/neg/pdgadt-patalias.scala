trait Expr { type X }
trait IntExpr { type X = Int }

val iexpr: Expr { type X = Int } = new Expr { type X = Int }

object Zero { type X = 0 }

def direct1(x: Expr) = x match {
  case _: iexpr.type =>
    val x1: Int = ??? : x.X
    val x2: x.X = ??? : Int
    val x3: iexpr.type = x
}

def direct2(x: Expr) = x match {
  case _: Zero.type =>
    val x1: Int = ??? : x.X  // limitation // error
    val x2: x.X = 0  // limitation // error
}

def indirect1(a: Expr, b: IntExpr) = a match {
  case r: b.type =>
    val x1: Int = ??? : a.X
    val x2: a.X = ??? : Int
    val x3: a.type = b
}

def indirect2(a: Expr, b: Expr) = a match {
  case _: IntExpr =>
    // sanity check
    val x1: Int = ??? : a.X
    val x2: a.X = ??? : Int
    b match {
      case _: a.type =>
        val x1: Int = ??? : b.X
        val x2: b.X = ??? : Int
    }
}
