import scala.quoted._

object Test {
  val tb = Toolbox.make(getClass.getClassLoader)
  def eval1(ff: Expr[Int => Int]): Staged[Int] = ff('{42})

  def peval1(): Staged[Unit] = '{
    def f(x: Int): Int = ${eval1('f)}
  }

  def main(args: Array[String]): Unit = {
    println(tb.show {
      val p = peval1()
      p
    })
  }

}