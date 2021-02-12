import scala.quoted.*
import scala.quoted.staging.*

object Test {
  given Compiler = Compiler.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = {

    println("start")

    run {
      println("start e")
      val e = '{
        ${
          println("splice")
          '{3}
        }
      }
      println("end e")
      e
    }

    println("end")
  }
}
