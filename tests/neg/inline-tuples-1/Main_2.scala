
object Test {

  def main(args: Array[String]): Unit = {
    val t1 = Tuple1(1)
    val t2 = Tuple2(1, 2)
    val t3 = Tuple3(1, 2, 3)
    val t4 = Tuple4(1, 2, 3, 4)
    val t5 = Tuple5(1, 2, 3, 4, 5)
    val t6 = Tuple6(1, 2, 3, 4, 5, 6)
    val t7 = Tuple7(1, 2, 3, 4, 5, 6, 7)
    val t8 = Tuple8(1, 2, 3, 4, 5, 6, 7, 8)
    val t9 = Tuple9(1, 2, 3, 4, 5, 6, 7, 8, 9)
    val t10 = Tuple10(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
    val t11 = Tuple11(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11)
    val t12 = Tuple12(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12)
    val t13 = Tuple13(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
    val t14 = Tuple14(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14)
    val t15 = Tuple15(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15)
    val t16 = Tuple16(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    val t17 = Tuple17(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17)
    val t18 = Tuple18(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18)
    val t19 = Tuple19(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19)
    val t20 = Tuple20(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20)
    val t21 = Tuple21(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21)
    val t22 = Tuple22(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22)
    println(sum(t1)) // error
    println(sum(t2)) // error
    println(sum(t3)) // error
    println(sum(t4)) // error
    println(sum(t5)) // error
    println(sum(t6)) // error
    println(sum(t7)) // error
    println(sum(t8)) // error
    println(sum(t9)) // error
    println(sum(t10)) // error
    println(sum(t11)) // error
    println(sum(t12)) // error
    println(sum(t13)) // error
    println(sum(t14)) // error
    println(sum(t15)) // error
    println(sum(t16)) // error
    println(sum(t17)) // error
    println(sum(t18)) // error
    println(sum(t19)) // error
    println(sum(t20)) // error
    println(sum(t21)) // error
    println(sum(t22)) // error

    val a: Int = 1
    println(sum(Tuple1(
      a // error
    )))
    println(sum(Tuple2(
      a, // error
      a // error
    )))
    println(sum(Tuple3(
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple4(
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple5(
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple6(
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple7(
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple8(
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple9(
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple10(
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple11(
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple12(
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple13(
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple14(
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple15(
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple16(
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple17(
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple18(
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple19(
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple20(
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple21(
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
    println(sum(Tuple22(
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a, // error
      a // error
    )))
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
