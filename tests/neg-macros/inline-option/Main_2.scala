
object Main {
  val a: Int = 3
  size(Some(a)) // error

  val b: Option[Int] = Some(4)
  size(b) // error

  inline def size(inline opt: Option[Int]): Int = ${ Macro.impl(opt) }

  inline def size2(inline i: Int): Int = ${ Macro.impl(None) }

  inline def size3(inline i: Int): Int = ${ Macro.impl(Some(i)) }
  
}