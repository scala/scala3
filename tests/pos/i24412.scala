object test {
  import scala.compiletime.erasedValue

  inline def contains[T <: Tuple, E]: Boolean = inline erasedValue[T] match {
    case _: EmptyTuple => false
    case _: (_ *: tail) => contains[tail, E]
  }
  inline def check[T <: Tuple]: Unit = {
    inline if contains[T, Long] && false then ???
  }

  check[(String, Double)]
}
