import scala.quoted._, staging._

given Compiler = Compiler.make(getClass.getClassLoader)

val f: Array[Int] => Int = run {
  val stagedSum: Expr[Array[Int] => Int] = '{ (arr: Array[Int]) => 6 }
  println(stagedSum.show)
  stagedSum
}

object Main {
  def main(args: Array[String]): Unit =
    f.apply(Array(1, 2, 3)) // Returns 6
}
