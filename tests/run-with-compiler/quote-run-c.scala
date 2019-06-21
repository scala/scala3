
import scala.quoted._

object Test {
  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)
    val classExpr = '{
      class A {
        override def toString: String = "Foo"
      }
      new A
    }
    val classExpr2 = '{
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
