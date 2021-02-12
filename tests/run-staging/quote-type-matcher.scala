import scala.quoted.*
import scala.quoted.staging.*
import scala.reflect.ClassTag

object Test {
  given Compiler = Compiler.make(this.getClass.getClassLoader)
  def main(args: Array[String]): Unit = withQuotes {
    val '[List[Int]] = Type.of[List[Int]]

    Type.of[List[Int]] match
      case '[List[int]] =>
        println(Type.show[int])
        println()

    Type.of[Int => Double] match
      case  '[Function1[t1, r]] =>
        println(Type.show[t1])
        println(Type.show[r])
        println()

    Type.of[(Int => Short) => Double] match
      case '[Function1[Function1[t1, r0], r]] =>
        println(Type.show[t1])
        println(Type.show[r0])
        println(Type.show[r])

  }
}
