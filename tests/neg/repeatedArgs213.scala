import scala.collection.{immutable, mutable}
import java.nio.file.Paths

class repeatedArgs {
  def bar(xs: String*): Int = xs.length

  def test(xs: immutable.Seq[String], ys: collection.Seq[String], zs: Array[String]): Unit = {
    bar("a", "b", "c")
    bar(xs*)
    bar(ys*) // error: immutable.Seq expected, found Seq
    bar(zs*) // old-error: Remove (compiler generated) Array to Seq conversion in 2.13?

    Paths.get("Hello", "World")
    Paths.get("Hello", xs*)
    Paths.get("Hello", ys*) // error: immutable.Seq expected, found Seq
    Paths.get("Hello", zs*)
  }

  def test2(xs: immutable.Seq[String] | Null, ys: collection.Seq[String] | Null, zs: Array[String] | Null): Unit = {
    bar("a", "b", "c")
    bar(xs*)
    bar(ys*) // error: immutable.Seq expected, found Seq
    bar(zs*) // old-error: Remove (compiler generated) Array to Seq conversion in 2.13?

    Paths.get("Hello", "World")
    Paths.get("Hello", xs*)
    Paths.get("Hello", ys*) // error: immutable.Seq expected, found Seq
    Paths.get("Hello", zs*)
  }
}
