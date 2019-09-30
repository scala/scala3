import scala.quoted._

object TestB {
  def testB()(given QuoteContext): Expr[Int] = {
    TestA.testA()
    '{5}
  }
}
