sealed trait Expr[A <: Expr[A]]

case class Equals[A <: Value[A]](lhs: Expr[A], rhs: Expr[A]) extends Expr[Value.Boolean]

sealed trait Value[A <: Value[A]] extends Expr[A]
object Value {
  case class String(value: java.lang.String) extends Value[String]
  case class Int(value: Long) extends Value[Int]
  case class Boolean(value: java.lang.Boolean) extends Value[Boolean]
}

object Main extends App {
  def evaluate[A <: Value[A]](expr: Expr[A]): A = expr match {
    case Equals(Value.String(lhs), Value.String(rhs)) => Value.Boolean(lhs == rhs)
  }
}