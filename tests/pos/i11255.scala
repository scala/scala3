class A
class B extends A

object O:
  opaque type Id[T] = T
  extension [T](id: Id[T]) def get: T = id
  def f[S <: A, T](ff: S => T): Id[S => T] = ???
  def g[S <: A, T](ff: S => T): Option[S => T] = ???
  def h[S, T](ff: S => T): Id[S => T] = ???

object example:
  import O._

  val a = new A
  val b = new B

  val f1 = f((a: A) => 0)
  f1.get.apply(a)
  val f2 = f((b: B) => 0)
  f2.get.apply(b)

  val g1 = g((a: A) => 0)
  g1.get.apply(a)

  val h1 = h((a: A) => 0)
  h1.get.apply(a)
