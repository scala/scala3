import scala.collection.{immutable, mutable}
import java.nio.file.Paths
import java.util.concurrent.ForkJoinTask

class repeatedArgs {
  def bar(xs: String*): Int = xs.length

  def test(xs: immutable.Seq[String], ys: collection.Seq[String], zs: Array[String]): Unit = {
    bar("a", "b", "c")
    bar(xs: _*)
    bar(ys: _*) // error in 2.13
    bar(zs: _*)

    Paths.get("Hello", "World")
    Paths.get("Hello", xs: _*)
    Paths.get("Hello", ys: _*) // error in 2.13
    Paths.get("Hello", zs: _*)

    val List(_, others: _*) = xs.toList // toList should not be needed, see #4790
    val x: collection.Seq[String] = others
    // val y: immutable.Seq[String] = others // ok in 2.13
  }

  def invokeAll[T](tasks: ForkJoinTask[T]*): Unit = ForkJoinTask.invokeAll(tasks: _*)
  def invokeAll2(tasks: ForkJoinTask[_]*): Unit = ForkJoinTask.invokeAll(tasks: _*)
}
