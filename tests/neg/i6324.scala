import quoted._
import tasty._

def res(x: quoted.Expr[Int]) given tasty.Reflection: quoted.Expr[Int] = x match {
  case '{ 1 + $b } => b  // error // error
}