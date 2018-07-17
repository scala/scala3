import scala.collection.{immutable, mutable}
import java.nio.file.Paths

/** Compiled with `-Yimmutable-seq` */
class repeatedArgs {
  def bar(xs: String*): Int = xs.length

  def test(xs: immutable.Seq[String]): Unit = {
    bar("a", "b", "c")
    bar(xs: _*)

    Paths.get("Hello", "World")
    Paths.get("Hello", xs: _*)

    val List(_, others: _*) = xs.toList // toList should not be needed, see #4790
    val x: immutable.Seq[String] = others
  }
}
