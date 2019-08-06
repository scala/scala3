import scala.quoted._
import scala.reflect.ClassTag

object Test {
  implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(this.getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    val '[List[Int]] = '[List[Int]]

    val '[List[$int]] = '[List[Int]]
    println(int.show)
    println()

    {
      val '[Function1[$t1, $r]] = '[Int => Double]
      println(t1.show)
      println(r.show)
      println()
    }

    {
      val '[Function1[Function1[$t1, $r0], $r]] = '[(Int => Short) => Double]
      println(t1.show)
      println(r0.show)
      println(r.show)
    }
  }
}
