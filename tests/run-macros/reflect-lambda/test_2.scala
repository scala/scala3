object Test {
  import lib.*

  case class IntList(args: Int*) {
    def exists(f: Int => Boolean): Boolean = args.exists(f)
  }

  def main(args: Array[String]): Unit = {
    assert(IntList(3, 5).exists(_ == 3))
    assert(IntList(3, 5).exists(5 == _))
    assert(IntList(3, 5).exists(x => x == 3))
    assert(IntList(3, 5).exists(x => 5 == x))
  }
}
