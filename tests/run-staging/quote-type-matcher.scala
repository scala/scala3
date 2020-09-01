import scala.quoted._
import scala.quoted.staging._
import scala.reflect.ClassTag

object Test {
  given Toolbox = Toolbox.make(this.getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuoteContext {
    val '[List[Int]] = Type[List[Int]]

    Type[List[Int]] match
      case '[List[$int]] =>
        println(int.show)
        println()

    Type[Int => Double] match
      case  '[Function1[$t1, $r]] =>
        println(t1.show)
        println(r.show)
        println()

    Type[(Int => Short) => Double] match
      case '[Function1[Function1[$t1, $r0], $r]] =>
        println(t1.show)
        println(r0.show)
        println(r.show)

  }
}
