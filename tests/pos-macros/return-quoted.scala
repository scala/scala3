import scala.quoted._

def test1(using s: Scope): s.Expr[Int] =
  return '{1}

def test2(using s: Scope): s.Expr[Int] =
  return (??? : s.Expr[Int])
