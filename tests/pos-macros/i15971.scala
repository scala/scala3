sealed trait A
case class B[T](x: T) extends A

object Test {
  inline def fun(x: A): Int = inline x match {
    case B(x1): B[t] => 0
  }

  @main def main() = 
    val x = B(0)
    fun(x)
}