
object Test extends App {
  def assertNew[T, U](expr: => IArray[T], original: IArray[U]): Unit = {
    val result = expr
    assert(result ne original, "IArray was mutated in place")
    println(result)
  }

  val arr = Array[Int](1,2,3,4,5,6,7,8,9,10)

  assertNew(arr.filter(_ % 2 == 0), arr)
}
