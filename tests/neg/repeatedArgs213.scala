import scala.collection.{immutable, mutable}
import java.nio.file.Paths

class repeatedArgs {
  def bar(xs: String*): Int = xs.length

  def test(xs: immutable.Seq[String], ys: collection.Seq[String], zs: Array[String]): Unit = {
    bar("a", "b", "c")
    bar(xs: _*)
    bar(ys: _*) // error: immutable.Seq expected, found Seq
    bar(zs: _*) // old-error: Remove (compiler generated) Array to Seq convertion in 2.13?

    Paths.get("Hello", "World")
    Paths.get("Hello", xs: _*)
    Paths.get("Hello", ys: _*) // error: immutable.Seq expected, found Seq
    Paths.get("Hello", zs: _*)
  }
}
