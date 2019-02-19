
object Test {

  def main(args: Array[String]): Unit = {
    println(size(None))
    println(size(Some(1)))
    println(size(new Some(2)))

    println(size2(3))
    println(size3(4))
    println(size4(5))

    println(size5(None))
    println(size5(Some(None)))
    println(size5(Some(Some(6))))
  }

  inline def size(inline opt: Option[Int]): Int = ${ Macros.impl(opt) }

  inline def size2(inline i: Int): Int = ${ Macros.impl(None) }

  inline def size3(inline i: Int): Int = ${ Macros.impl(Some(i)) }

  inline def size4(inline i: Int): Int = ${ Macros.impl2(Some(Some(i))) }

  inline def size5(inline opt: Option[Option[Int]]): Int = ${ Macros.impl2(opt) }

}
