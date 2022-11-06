// scalac: -Wunused:patvars

sealed trait Calc
sealed trait Const extends Calc
case class Sum(a: Calc, b: Calc) extends Calc
case class S(pred: Const) extends Const
case object Z extends Const

val a = Sum(S(S(Z)),Z) match {
  case Sum(a,Z) => Z // error
  case Sum(a@S(_),Z) => Z // error
  case Sum(a@S(_),Z) => a // OK
  case Sum(a@S(b@S(_)), Z) => a // error
  case Sum(a@S(b@(S(_))), Z) => Sum(a,b) // OK
  case Sum(_,_) => Z // OK
  case _ => Z // OK
}
