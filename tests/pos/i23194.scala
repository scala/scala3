class R[T] extends annotation.StaticAnnotation

class A[T]:
  val next: A[T] = null
  val self: this.type = this
  val selfnext: this.next.type = this.next
  def f: (A[T] @R[this.type], A[T] @R[this.next.type]) = ???
  def g: (A[T] @R[self.type], A[T] @R[selfnext.type]) = ???

class Test:
  def test =
    val (a, b) = A[String]().f
    val (a2, b2) = A[String]().g
  