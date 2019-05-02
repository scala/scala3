
object Test {

  import Macros._

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

}
