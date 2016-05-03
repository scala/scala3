// yet another variant, testing super accessors
// (but exhibited a crash in RefChecks).

trait T {
  def foo[B](x: C[B]): C[B]
}
abstract class A extends T {
  type C[X]
  def foo[B](x: C[B]): C[B]       = {println("A.C"); x}
  def foo[B](x: List[B]): List[B] = {println("A.List"); x}
}
trait U extends T {
  abstract override def foo[B](x: C[B]): C[B] = super.foo[B](x)
}
object Test extends A with U {
  type C[X] = List[X]
}
