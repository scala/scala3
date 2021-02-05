import compiletime.uninitialized
object Test {
  var x1: Int => Float => Double = uninitialized
  var x2: (Int => Float) => Double = uninitialized
  var x3: Int => Double

  def main(args: Array[String]): Unit = {
    x1 = "a"
    x2 = "b"
    x3 = "c"
  }
}
