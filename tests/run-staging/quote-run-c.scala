
import scala.quoted.*
import scala.quoted.staging.*

object Test {
  def main(args: Array[String]): Unit = {
    given Compiler = Compiler.make(getClass.getClassLoader)
    def classExpr(using Quotes) = '{
      class A {
        override def toString: String = "Foo"
      }
      new A
    }
    def classExpr2(using Quotes) = '{
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
