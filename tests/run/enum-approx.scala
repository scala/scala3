enum Fun[-T, +U >: Null] {
  def f: (T => U)|Null = this match {
    case Identity(g) => g
    case ConstNull => (_ => null)
    case ConstNullClass() => (_ => null)
    case ConstNullSimple => null
  }

  case Identity[T, U >: Null](g: T => U) extends Fun[T, U]
  case ConstNull
  case ConstNullClass()
  case ConstNullSimple
}

object Test {
  def main(args: Array[String]) = {
    val x: Null = Fun.ConstNull.f.nn("abc")
    val y: Null = Fun.ConstNullClass().f.nn("abc")
    assert(Fun.ConstNullSimple.f == null)
  }
}

