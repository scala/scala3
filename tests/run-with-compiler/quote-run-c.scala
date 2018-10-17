
import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    val tb: Toolbox = Toolbox.make
    import tb.run

    val classExpr: Staged[Any] = '{
      class A {
        override def toString: String = "Foo"
      }
      new A
    }
    val classExpr2: Staged[Any] = '{
      class A {
        override def toString: String = "Bar"
      }
      new A
    }
    println(run(classExpr))
    println(run(classExpr).getClass == run(classExpr).getClass)
    println(run(classExpr2))
    println(run(classExpr2).getClass)
  }
}
