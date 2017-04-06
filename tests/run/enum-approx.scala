enum class Fun[-T, +U >: Null] {
  def f: T => U = null
}
object Fun {
  case Identity[T, U >: Null](override val f: T => U) extends Fun[T, U]
  case ConstNull {
    override def f = x => null
  }
  case ConstNullClass() {
    override def f = x => null
  }
  case ConstNullSimple
}

object Test {
  def main(args: Array[String]) = {
    val x: Null = Fun.ConstNull.f("abc")
    val y: Null = Fun.ConstNullClass().f("abc")
    assert(Fun.ConstNullSimple.f == null)
  }
}

