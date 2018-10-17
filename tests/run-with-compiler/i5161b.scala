import scala.quoted._

object Test {
  val toolbox: Toolbox = Toolbox.make

  def main(args: Array[String]): Unit = {
    def res: Staged[Any] = '{
      val x: Option[Int] = Option(3)
      if (x.isInstanceOf[Some[_]]) Option(1)
      else None
    }
    println("show0 : " + toolbox.show(res))
    println("run1 : " + toolbox.run(res))
    println("run2 : " + toolbox.run(res))
    println("show3 : " + toolbox.show(res))
  }
}

