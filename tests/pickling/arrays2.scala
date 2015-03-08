package arrays

case class C();

object arrays2 {

  def main(args: Array[String]): Unit = {
    val a: Array[Array[C]] = new Array[Array[C]](2);
    a(0) = new Array[C](2);
    a(0)(0) = new C();
  }
}

// #2422
object arrays4 {
  val args = Array[String]("World")
  "Hello %1$s".format(args: _*)
}

// #2461
object arrays3 {
  import scala.collection.JavaConversions._
  def apply[X](xs : X*) : java.util.List[X] = java.util.Arrays.asList(xs: _*)

  def apply1[X <: String](xs : X*) : java.util.List[X] = java.util.Arrays.asList(xs: _*)
  def apply2[X <: AnyVal](xs : X*) : java.util.List[X] = java.util.Arrays.asList(xs: _*)
  def apply3(xs : Int*) : java.util.List[Int] = java.util.Arrays.asList(xs: _*)
  def apply4(xs : Unit*) : java.util.List[Unit] = java.util.Arrays.asList(xs: _*)
  def apply5(xs : Null*) : java.util.List[Null] = java.util.Arrays.asList(xs: _*)
  def apply6(xs : Nothing*) : java.util.List[Nothing] = java.util.Arrays.asList(xs: _*)
}

