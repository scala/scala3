import scala.quoted._
def impl(using Quotes): Expr[Unit] =
  '{ ([V] => (v: V) => println(v)).apply[Int](2) }
