//> using options -Xfatal-warnings -Wunused:unsafe-warn-patvars
// todo : change to :patvars

sealed trait Calc
sealed trait Const extends Calc
case class Sum(a: Calc, b: Calc) extends Calc
case class S(pred: Const) extends Const
case object Z extends Const

val a = Sum(S(S(Z)),Z) match {
  case Sum(a,Z) => Z // error
  // case Sum(a @ _,Z) => Z // todo : this should pass in the future
  case Sum(a@S(_),Z) => Z // error
  case Sum(a@S(_),Z) => a // OK
  case Sum(a@S(b@S(_)), Z) => a // error
  case Sum(a@S(b@S(_)), Z) => a // error
  case Sum(a@S(b@(S(_))), Z) => Sum(a,b) // OK
  case Sum(_,_) => Z // OK
  case _ => Z // OK
}

// todo : This should pass in the future
// val b = for {
//   case Some(x) <- Option(Option(1))
// } println(s"$x")

// todo : This should *NOT* pass in the future
// val c = for {
//   case Some(x) <- Option(Option(1))
// } println(s"hello world")
