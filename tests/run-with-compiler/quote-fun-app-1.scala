import scala.quoted._

object Test {

  def main(args: Array[String]): Unit = {
    val tb = Toolbox.make
    val f = tb.run(f1)
    println(f(42))
    println(f(43))
  }

  def f1(implicit st: StagingContext): Expr[Int => Int] = '{ n => ${f2(implicitly)('n)} }
  def f2(implicit st: StagingContext): Expr[Int => Int] = '{ n => ${f3(implicitly)('n)} }
  def f3(implicit st: StagingContext): Expr[Int => Int] = '{ n => ${f4(implicitly)('n)} }
  def f4(implicit st: StagingContext): Expr[Int => Int] = '{ n => n }
}
