// infers wrong instance --> an implementatioin is missing
trait Ord[-T] {
  def less(x: T, y: T): Boolean
}

object Test {

  implicit val anyIsOrd: Ord[Any] = new Ord[Any] {
    def less(x: Any, y: Any): Boolean = ???
  }

  implicit val intIsOrd: Ord[Int] = new Ord[Int] {
    def less(x: Int, y: Int): Boolean = x < y
  }

  def less[T: Ord](x: T, y: T): Boolean =
    implicitly[Ord[T]].less(x, y)

  def main(args: Array[String]) =
    assert(less(1, 2))

}
