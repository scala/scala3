import scala.internal.StagedTuple._

object Test {

  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

    assert(fromArrayStaged('{ Array.empty[Object] }, Some(0)).run.==(()))
    assert(fromArrayStaged[Tuple1[String]]('{ Array[Object]("a") }, Some(1)).run == Tuple1("a"))
    assert(fromArrayStaged[(String, String)]('{ Array[Object]("a", "b") }, Some(2)).run == ("a", "b"))
    assert(fromArrayStaged[(String, String, String)]('{ Array[Object]("a", "b", "c") }, Some(3)).run == ("a", "b", "c"))

    assert(headStaged[Tuple1[String]]('{ Tuple1("a") }, Some(1)).run == "a")
    assert(headStaged[(String, String)]('{ ("a", "b") }, Some(2)).run == "a")
    assert(headStaged[(String, String, String)]('{ ("a", "b", "c") }, Some(3)).run == "a")

    assert(tailStaged[Tuple1[String]]('{ Tuple1("a") }, Some(1)).run == (()))
    assert(tailStaged[(String, String)]('{ ("a", "b") }, Some(2)).run == Tuple1("b"))
    assert(tailStaged[(String, String, String)]('{ ("a", "b", "c") }, Some(3)).run == ("b", "c"))

    assert(headStaged[(String, String, String)]('{ ("a", "b", "c") }, Some(3)).run == "a")
    assert(headStaged[(String, String, String)]('{ ("a", "b", "c") }, Some(3)).run == "a")
    assert(headStaged[(String, String, String)]('{ ("a", "b", "c") }, Some(3)).run == "a")

  }
}
