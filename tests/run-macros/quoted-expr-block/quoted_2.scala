
object Test {
  def main(args: Array[String]): Unit = {
    var a = 0
    replicate(500, a += 1)
    assert(a == 500, a)
  }
}
