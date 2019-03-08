
object Test {

  def main(args: Array[String]): Unit = {
    println(sum(Tuple1(1)))
    println(sum(Tuple2(1, 2)))
    println(sum(Tuple3(1, 2, 3)))
    println(sum(Tuple4(1, 2, 3, 4)))
    println(sum(Tuple5(1, 2, 3, 4, 5)))
    println(sum(Tuple6(1, 2, 3, 4, 5, 6)))
    println(sum(Tuple7(1, 2, 3, 4, 5, 6, 7)))
    println(sum(Tuple8(1, 2, 3, 4, 5, 6, 7, 8)))
    println(sum(Tuple9(1, 2, 3, 4, 5, 6, 7, 8, 9)))
    println(sum(Tuple10(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
    println(sum(Tuple11(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)))
    println(sum(Tuple12(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)))
    println(sum(Tuple13(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)))
    println(sum(Tuple14(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)))
    println(sum(Tuple15(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)))
    println(sum(Tuple16(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)))
    println(sum(Tuple17(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)))
    println(sum(Tuple18(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)))
    println(sum(Tuple19(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)))
    println(sum(Tuple20(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)))
    println(sum(Tuple21(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)))
    println(sum(Tuple22(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)))
  }

  inline def sum(inline tup: Tuple1[Int]): Int = ${ Macros.tup1(tup) }
  inline def sum(inline tup: Tuple2[Int, Int]): Int = ${ Macros.tup2(tup) }
  inline def sum(inline tup: Tuple3[Int, Int, Int]): Int = ${ Macros.tup3(tup) }
  inline def sum(inline tup: Tuple4[Int, Int, Int, Int]): Int = ${ Macros.tup4(tup) }
  inline def sum(inline tup: Tuple5[Int, Int, Int, Int, Int]): Int = ${ Macros.tup5(tup) }
  inline def sum(inline tup: Tuple6[Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup6(tup) }
  inline def sum(inline tup: Tuple7[Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup7(tup) }
  inline def sum(inline tup: Tuple8[Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup8(tup) }
  inline def sum(inline tup: Tuple9[Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup9(tup) }
  inline def sum(inline tup: Tuple10[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup10(tup) }
  inline def sum(inline tup: Tuple11[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup11(tup) }
  inline def sum(inline tup: Tuple12[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup12(tup) }
  inline def sum(inline tup: Tuple13[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup13(tup) }
  inline def sum(inline tup: Tuple14[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup14(tup) }
  inline def sum(inline tup: Tuple15[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup15(tup) }
  inline def sum(inline tup: Tuple16[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup16(tup) }
  inline def sum(inline tup: Tuple17[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup17(tup) }
  inline def sum(inline tup: Tuple18[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup18(tup) }
  inline def sum(inline tup: Tuple19[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup19(tup) }
  inline def sum(inline tup: Tuple20[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup20(tup) }
  inline def sum(inline tup: Tuple21[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup21(tup) }
  inline def sum(inline tup: Tuple22[Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int, Int]): Int = ${ Macros.tup22(tup) }

}
