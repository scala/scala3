import scala.quoted._
import scala.quoted.staging._

object Test {
  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)

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
