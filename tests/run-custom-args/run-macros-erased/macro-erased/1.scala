import scala.quoted._

object Macro {
  inline def foo1(i: Int) = $ { case1('{ i }) }
  inline def foo2(i: Int) = $ { case2(1)('{ i }) }
  inline def foo3(i: Int) = $ { case3('{ i })(1) }
  inline def foo4(i: Int) = $ { case4(1)('{ i }, '{ i }) }
  inline def foo5(i: Int) = $ { case5('{ i }, '{ i })(1) }
  inline def foo6(i: Int) = $ { case6(1)('{ i })('{ i }) }
  inline def foo7(i: Int) = $ { case7('{ i })(1)('{ i }) }
  inline def foo8(i: Int) = $ { case8('{ i })('{ i })(1) }

  def case1(using s: Scope)(erased i: s.Expr[Int]): s.Expr[Int] = '{ 0 }
  def case2(using s: Scope) (i: Int)(erased j: s.Expr[Int]): s.Expr[Int] = '{ 0 }
  def case3(using s: Scope)(erased i: s.Expr[Int]) (j: Int): s.Expr[Int] = '{ 0 }
  def case4(using s: Scope) (h: Int)(erased i: s.Expr[Int], j: s.Expr[Int]): s.Expr[Int] = '{ 0 }
  def case5(using s: Scope)(erased i: s.Expr[Int], j: s.Expr[Int]) (h: Int): s.Expr[Int] = '{ 0 }
  def case6(using s: Scope) (h: Int)(erased i: s.Expr[Int])(erased j: s.Expr[Int]): s.Expr[Int] = '{ 0 }
  def case7(using s: Scope)(erased i: s.Expr[Int]) (h: Int)(erased j: s.Expr[Int]): s.Expr[Int] = '{ 0 }
  def case8(using s: Scope)(erased i: s.Expr[Int])(erased j: s.Expr[Int]) (h: Int): s.Expr[Int] = '{ 0 }
}
