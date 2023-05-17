package hello

import scala.util.Right

enum Color:
  case Red, Green, Blue

object HelloWorld:
  def main(args: Array[String]): Unit = {
    println("hello 2.13 library bootstrapped")
    println(Color.Red)
    println(Color.Green)
    println(Color.Blue)

    testScala2UnapplySignatures()
  }

  def testScala2UnapplySignatures() = {
    val _: Option[Int] = Some.unapply(Some(1))
    val _: Option[Int] = Right.unapply(Right(1))
    val _: Option[(Int, List[Int])] = ::.unapply(::(1, Nil))

    val _: Option[Int] = Tuple1.unapply(Tuple1(1))
    val _: Option[(Int, Int)] = Tuple2.unapply((1, 2))
    val _: Option[(Int, Int, Int)] = Tuple3.unapply((1, 2, 3))
  }
