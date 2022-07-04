trait Expr { type T }

def foo[A <: Int](e: Expr { type T = A }) = e match
  case e1: Expr { type T <: Int } =>
    val i: Int = ??? : e1.T
