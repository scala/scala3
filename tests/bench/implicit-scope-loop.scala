trait Dummy[T]


trait A[T] extends B
trait B extends Dummy[A[Int]]
object B {
  implicit def theB: B = new B {}
  implicit def theA: A[Int] = new A[Int] {}
}

object Test {
  def getB(implicit b: B) = b
  def getA[T](implicit a: A[T]) = a

  getB
  getA
}