
import Macros.dealias

object Test {

  def main(args: Array[String]): Unit = {
    type A = String
    type B = List[A]
    type C[X] = List[X]
    println(dealias[String])
    println(dealias[A])
    println(dealias[B])
    println(dealias[C[Int]])
  }
}
