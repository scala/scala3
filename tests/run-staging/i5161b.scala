import scala.quoted.*
import scala.quoted.staging.*

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = {
    def res(using Quotes) = '{
      val x: Option[Int] = Option(3)
      if (x.isInstanceOf[Some[_]]) Option(1)
      else None
    }
    println("show0 : " + withQuotes(res.show))
    println("run1 : " + run(res))
    println("run2 : " + run(res))
    println("show3 : " + withQuotes(res.show))
  }
}
