
object Test {

  def main(args: Array[String]): Unit = {
    println(get1(Tuple1(1)))
    println(get1(new Tuple1(2)))

    println(get2(4))
    println(get3(5))

    println(get4(Tuple1(Tuple1(6))))
  }

  inline def get1(inline tup: Tuple1[Int]): Int = ${ Macros.impl(tup) }

  inline def get2(inline i: Int): Int = ${ Macros.impl(Tuple1(i)) }

  inline def get3(inline i: Int): Int = ${ Macros.impl2(Tuple1(Tuple1(i))) }

  inline def get4(inline tup: Tuple1[Tuple1[Int]]): Int = ${ Macros.impl2(tup) }

}
