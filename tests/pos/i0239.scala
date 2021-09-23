package i0239

package p {
  class C[A] {
    implicit def foo: M[A] = ???
  }

  object `package` extends C[String]

  object test0 {
    def compute[A](implicit m: M[A]): A = ???
    val v = compute
    val v1: String = v
  }
}

trait M[A]

object test1 {

  def compute[A](implicit m: M[A]): A = ???

  import p.*
  val v = compute
  val v1: String = v
}
