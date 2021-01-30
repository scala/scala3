import scala.quoted.*

object Macro {
  inline def foo1(i: Int) = $ { case1('{ i }) }
  inline def foo2(i: Int) = $ { case2(1)('{ i }) }
  inline def foo3(i: Int) = $ { case3('{ i })(1) }
  inline def foo4(i: Int) = $ { case4(1)('{ i }, '{ i }) }
  inline def foo5(i: Int) = $ { case5('{ i }, '{ i })(1) }
  inline def foo6(i: Int) = $ { case6(1)('{ i })('{ i }) }
  inline def foo7(i: Int) = $ { case7('{ i })(1)('{ i }) }
  inline def foo8(i: Int) = $ { case8('{ i })('{ i })(1) }

  def case1(erased i: Expr[Int])(using Quotes): Expr[Int] = '{ 0 }
  def case2 (i: Int)(erased j: Expr[Int])(using Quotes): Expr[Int] = '{ 0 }
  def case3(erased i: Expr[Int]) (j: Int)(using Quotes): Expr[Int] = '{ 0 }
  def case4 (h: Int)(erased i: Expr[Int], j: Expr[Int])(using Quotes): Expr[Int] = '{ 0 }
  def case5(erased i: Expr[Int], j: Expr[Int]) (h: Int)(using Quotes): Expr[Int] = '{ 0 }
  def case6 (h: Int)(erased i: Expr[Int])(erased j: Expr[Int])(using Quotes): Expr[Int] = '{ 0 }
  def case7(erased i: Expr[Int]) (h: Int)(erased j: Expr[Int])(using Quotes): Expr[Int] = '{ 0 }
  def case8(erased i: Expr[Int])(erased j: Expr[Int]) (h: Int)(using Quotes): Expr[Int] = '{ 0 }
}
