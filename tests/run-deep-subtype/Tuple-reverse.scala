//> using options -experimental

import scala.reflect.ClassTag

object Test {
  def main(args: Array[String]): Unit = {

    def testArray[T: ClassTag](n: Int, elem: Int => T): Unit = {
      val t: Int *: Tuple = 0 *: Tuple.fromArray(Array.tabulate(n)(elem))
      println(t.reverse)
    }

    for (i <- 0 to 25)
      testArray(i, j => j)

    val tuple: Tuple3[Int, Boolean, String] = (1, true, "hello")
    val reversedTuple: Tuple3[String, Boolean, Int] = tuple.reverse

    assert(reversedTuple == ("hello", true, 1))

    println(EmptyTuple.reverse)
    println(Tuple1(1).reverse)
    println((1, 2).reverse)
    println((1, 2, 3).reverse)
    println((1, 2, 3, 4).reverse)
    println((1, 2, 3, 4, 5).reverse)
    println((1, 2, 3, 4, 5, 6).reverse)
    println((1, 2, 3, 4, 5, 6, 7).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8, 9).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8, 9, 10).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24).reverse)
    println((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25).reverse)

    println(Tuple().reverse)
    println((1 *: Tuple()).reverse)
    println((1 *: 2 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: 14 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: 14 *: 15 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: 14 *: 15 *: 16 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: 14 *: 15 *: 16 *: 17 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: 14 *: 15 *: 16 *: 17 *: 18 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: 14 *: 15 *: 16 *: 17 *: 18 *: 19 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: 14 *: 15 *: 16 *: 17 *: 18 *: 19 *: 20 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: 14 *: 15 *: 16 *: 17 *: 18 *: 19 *: 20 *: 21 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: 14 *: 15 *: 16 *: 17 *: 18 *: 19 *: 20 *: 21 *: 22 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: 14 *: 15 *: 16 *: 17 *: 18 *: 19 *: 20 *: 21 *: 22 *: 23 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: 14 *: 15 *: 16 *: 17 *: 18 *: 19 *: 20 *: 21 *: 22 *: 23 *: 24 *: Tuple()).reverse)
    println((1 *: 2 *: 3 *: 4 *: 5 *: 6 *: 7 *: 8 *: 9 *: 10 *: 11 *: 12 *: 13 *: 14 *: 15 *: 16 *: 17 *: 18 *: 19 *: 20 *: 21 *: 22 *: 23 *: 24 *: 25 *: Tuple()).reverse)
  }
}
