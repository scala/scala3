import scala.quoted._

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

  enum Exp {
    case Int2(x: Int)
    case Add(e1: Exp, e2: Exp)
  }
  import Exp._

  def evalTest(e: Exp): Expr[Option[Int]] = e match {
    case Int2(x) => '{ Some(${x.toExpr}) }
    case Add(e1, e2) =>
     '{
        (${evalTest(e1)}, ${evalTest(e2)}) match {
        case (Some(x), Some(y)) => Some(x+y)
        case _ => None
        }
      }
    case null => '{ None }
  }


  def main(args: Array[String]): Unit = {
    val test = Add(Int2(1), Int2(1))
    val res = evalTest(test)
    println("run : " + run(res))
    println("show : " + withQuoteContext(res.show))
  }
}
