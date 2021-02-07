import scala.collection.{immutable, mutable}
import java.nio.file.Paths

class repeatedArgs {
  def bar(xs: String*): Int = bat(xs)
  def bat(xs: immutable.Seq[String]) = xs.length

  def test(xs: immutable.Seq[String]): Unit = {
    bar("a", "b", "c")
    bar(xs*)

    Paths.get("Hello", "World")
    Paths.get("Hello", xs*)

    val List(_, others*) = xs.toList // toList should not be needed, see #4790
    val x: immutable.Seq[String] = others
  }

  def test2(xs: immutable.Seq[String] | Null): Unit = {
    bar("a", "b", "c")
    bar(xs*)

    Paths.get("Hello", "World")
    Paths.get("Hello", xs*)

    val List(_, others*) = xs.toList // toList should not be needed, see #4790
    val x: immutable.Seq[String] = others
  }
}
