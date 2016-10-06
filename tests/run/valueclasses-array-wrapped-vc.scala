class X(val y: Y) extends AnyVal
class Y(val z: Z) extends AnyVal
class Z(val x: Int) extends AnyVal
object Test {
  def main(args: Array[String]) = {
    val arr = Array(new X(new Y(new Z(55))), null)
    println(arr.toList)
    val arr2 = Array.ofDim[X](2,3,4)
    println(arr2.toList.size)
    val arr3 = new Array[X](5)
    println(arr3.toList)
    arr(1) = new X(new Y(new Z(77)))
    println(arr.toList)
    val e = arr(0)
    println(e)
  }
}
