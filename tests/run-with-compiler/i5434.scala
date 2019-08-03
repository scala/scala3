import scala.quoted._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

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
