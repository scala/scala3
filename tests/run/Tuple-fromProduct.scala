
object Test {
  def main(args: Array[String]): Unit = {

    def testProduct(product: Product): Unit = {
      val t: Tuple = Tuple.fromProduct(product)
      println(t)
    }

    testProduct(Tuple1(1))
    testProduct((1, 2))
    testProduct((1, 2, 3))
    testProduct((1, 2, 3, 4))
    testProduct((1, 2, 3, 4, 5))
    testProduct((1, 2, 3, 4, 5, 6))
    testProduct((1, 2, 3, 4, 5, 6, 7))
    testProduct((1, 2, 3, 4, 5, 6, 7, 8))
    testProduct((1, 2, 3, 4, 5, 6, 7, 8, 9))
    testProduct((1, 2, 3, 4, 5, 6, 7, 8, 9, 10))
    testProduct((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11))
    testProduct((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12))
    testProduct((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13))
    testProduct((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14))
    testProduct((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15))
    testProduct((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16))
    testProduct((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17))
    testProduct((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18))
    testProduct((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19))
    testProduct((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20))
    testProduct((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21))
    testProduct((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22))
    // FIXME ProductN for N > 22 does not extends product
//    testProduct((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23))
//    testProduct((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24))
//    testProduct((1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25))

    class MyProduct(val productArity: Int) extends Product {
      def productElement(n: Int): Any = n + 1
      def canEqual(that: Any): Boolean = false
    }

    for (i <- 1 to 25)
      testProduct(new MyProduct(i))

  }
}
