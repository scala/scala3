
import scala.quoted._

object Test {
  implicit val tbx: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = {
    val y: Expr[Unit] = '{
      val x: Expr[Unit] = '{println("bar")}
      println("foo")
      x.run
    }
    y.run
  }
}
