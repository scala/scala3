import quoted._
import scala.quoted.staging._

object Test {
  delegate for Toolbox = Toolbox.make(getClass.getClassLoader)
  def main(args: Array[String]): Unit = run {
    val q = '{
      val x = 1
      println(${
        println(1)
        val a = 'x
        val b = 'x
        '{ $a + $b }
      })
    }

    println(q.show)
    '{}
  }
}
