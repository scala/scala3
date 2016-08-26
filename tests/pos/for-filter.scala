object Test {

  case class C[T](xs: List[T]) {
    def filter(p: T => Boolean) = new C(xs.filter(p))
    def map[U](f: T => U) = new C(xs.map(f))
  }

  def main(args: Array[String]): Unit =
    println(for (x <- C(List(1, 2, 3)) if x % 2 == 0) yield x)
    // println(C(List(1, 2, 3)).withFilter(_ % 2 == 0))    // error

}
