import scala.quoted._

object Test {
  val toolbox = scala.quoted.Toolbox.make
  enum Exp {
    case Int2(x: Int)
    case Add(e1: Exp, e2: Exp)
  }
  import Exp._

  def evalTest(e: Exp): Staged[Option[Int]] = e match {
    case Int2(x) => '(Some(~x.toExpr))
    case Add(e1, e2) =>
      '{
        (~evalTest(e1), ~evalTest(e2)) match {
          case (Some(x), Some(y)) => Some(x+y)
          case _ => None
        }
      }
    case null => '(None)
  }


  def main(args: Array[String]): Unit = {
    val test = Add(Int2(1), Int2(1))
    def res: Staged[Option[Int]] = evalTest(test)
    println("run : " + toolbox.run(res))
    println("show : " + toolbox.show(res))
  }
}
