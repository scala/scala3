import scala.language.postfixOps

class Vector(values: Int*) {
  val data = values.toArray
  class Getter(i: Int) {
    def `>_=`(x: Int) =
      data(i) = x
    def > : Int =
      data(i)
  }
  def < (i:Int) = new Getter(i)
  override def toString = data.mkString("<", ", ", ">")
}

object Test {
  def main(args: Array[String]): Unit = {
    val v = new Vector(1, 2, 3)
    println(v) // prints <1, 2, 3>
    v<1> = 10 // assign 10 to element at index 1
    println(v) // prints <1, 10, 3>
    println(v<1>) // prints: value at 1 is 10
  }
}
