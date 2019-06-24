import scala.quoted._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

  def main(args: Array[String]): Unit = {
    val res = '{
      val x: Option[Int] = Option(3)
      if (x.isInstanceOf[Some[_]]) Option(1)
      else None
    }
    println("show0 : " + run(res.show.toExpr))
    println("run1 : " + run(res))
    println("run2 : " + run(res))
    println("show3 : " + run(res.show.toExpr))
  }
}
