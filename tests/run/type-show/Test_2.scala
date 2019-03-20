
object Test {
  import TypeToolbox._
  def main(args: Array[String]): Unit = {
    val x = 5
    assert(show[x.type] == "x.type")
    assert(show[Nil.type] == "scala.Nil.type")
    assert(show[Int] == "scala.Int")
    assert(show[Int => Int] == "scala.Function1[scala.Int, scala.Int]")
    assert(show[(Int, String)] == "scala.Tuple2[scala.Int, scala.Predef.String]")

    // TODO: more complex types:
    //  - implicit function types
    //  - dependent function types
    //  - refinement types
  }
}
