class Expr[T] {
  def unary_~ : T = ???
}
class Type[T] {
  type unary_~ = T
}
object Test {

  def f[T](t: Type[T], x: Expr[T]) = {
    val y: t.unary_~ = x.unary_~
    val z: ~t = ~x
  }

}
