
// Compile this with Scala 2.13
package i11173

trait DU[A, B]
trait H[F[_]]

trait Foo[E] {
  def foo: H[({type L[A] = DU[E, A]})#L]
}

trait Bar[E] extends Foo[E] {
  def bar = foo   // important note: return type not specified
}
