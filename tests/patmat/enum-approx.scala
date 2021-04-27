enum Fun[-T, +U >: Null] {
  def f: T => U = this match {
    case Identity(g) => g
    case ConstNull => (_ => null)
    case ConstNullClass(y) => (_ => null)
    case ConstNullSimple => null
  }

  case Identity[T, U >: Null](g: T => U) extends Fun[T, U]
  case ConstNull
  case ConstNullClass[T](x: T) extends Fun[T, Null]
  case ConstNullSimple
}

object Test {
  def main(args: Array[String]) = {
    val x: Null = Fun.ConstNull.f("abc")
    val y: Null = Fun.ConstNullClass("hello").f("abc")
    assert(Fun.ConstNullSimple.f == null)
  }

  import Fun.*

  def f[T, U >: Null](f: Fun[T, U]): T => U = f match {
    case Identity(g) => g
    case ConstNull => (_ => null)
    case ConstNullClass(y: Int) => (_ => null)
    case ConstNullSimple => null
  }
}
