enum Fun[-T, +U >: Null] {
  def f: T => U = this match {
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
    val x: Null = Fun.ConstNull.f("abc")
    val y: Null = Fun.ConstNullClass().f("abc")
    assert(Fun.ConstNullSimple.f == null)
  }
}

