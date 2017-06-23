import scala.collection.immutable._

object Test {
  def main(args: Array[String]): Unit = {
    val vec = Vector[Int](1, 3, 42)
    System.out.println(vec.map(0 until _).flatMap(x => x).sum)
    System.out.println(vec.fold(0)(_ + _))
  }
}
