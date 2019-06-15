import scala.quoted.run
import scala.internal.StagedTuple._

object Test {

  def main(args: Array[String]): Unit = {
    implicit val toolbox: scala.quoted.Toolbox = scala.quoted.Toolbox.make(getClass.getClassLoader)

    assert(run(fromArrayStaged('{ Array.empty[Object] }, Some(0))).==(()))
    assert(run(fromArrayStaged[Tuple1[String]]('{ Array[Object]("a") }, Some(1))) == Tuple1("a"))
    assert(run(fromArrayStaged[(String, String)]('{ Array[Object]("a", "b") }, Some(2))) == ("a", "b"))
    assert(run(fromArrayStaged[(String, String, String)]('{ Array[Object]("a", "b", "c") }, Some(3))) == ("a", "b", "c"))

    assert(run(headStaged[Tuple1[String]]('{ Tuple1("a") }, Some(1))) == "a")
    assert(run(headStaged[(String, String)]('{ ("a", "b") }, Some(2))) == "a")
    assert(run(headStaged[(String, String, String)]('{ ("a", "b", "c") }, Some(3))) == "a")

    assert(run(tailStaged[Tuple1[String]]('{ Tuple1("a") }, Some(1))) == (()))
    assert(run(tailStaged[(String, String)]('{ ("a", "b") }, Some(2))) == Tuple1("b"))
    assert(run(tailStaged[(String, String, String)]('{ ("a", "b", "c") }, Some(3))) == ("b", "c"))

    assert(run(headStaged[(String, String, String)]('{ ("a", "b", "c") }, Some(3))) == "a")
    assert(run(headStaged[(String, String, String)]('{ ("a", "b", "c") }, Some(3))) == "a")
    assert(run(headStaged[(String, String, String)]('{ ("a", "b", "c") }, Some(3))) == "a")

  }
}
