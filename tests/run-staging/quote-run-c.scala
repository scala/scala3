
import scala.quoted._
import scala.quoted.staging._

object Test {
  def main(args: Array[String]): Unit = {
    delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
    def classExpr given QuoteContext = '{
      class A {
        override def toString: String = "Foo"
      }
      new A
    }
    def classExpr2 given QuoteContext = '{
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
