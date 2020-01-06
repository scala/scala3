import scala.quoted._, staging._

given Toolbox = Toolbox.make(getClass.getClassLoader)

val f: Array[Int] => Int = run {
  val stagedSum: Expr[Array[Int] => Int] = '{ (arr: Array[Int]) => 6 }
  println(stagedSum.show)
  stagedSum
}

@main
def Test = {
  f.apply(Array(1, 2, 3)) // Returns 6
}
