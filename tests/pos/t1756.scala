trait Ring[T <: Ring[T]] {
  def +(that: T): T
  def *(that: T): T
}

class A extends Ring[A] {
  def +(that: A) = new A
  def *(that: A) = new A
}

class Poly[C <: Ring[C]](val c: C) extends Ring[Poly[C]] {
  def +(that: Poly[C]) = new Poly(this.c + that.c)
  def *(that: Poly[C]) = new Poly(this.c*that.c)
}

object Test extends App {

  implicit def coef2poly[CI <: Ring[CI]](c: CI): Poly[CI] = new Poly(c)

  val a = new A
  val x = new Poly(new A)

  println(x + a) // works
  println(a + x) // works

  val y = new Poly(new Poly(new A))

  println(x + y*x) // works
  println(x*y + x) // works
  println(y*x + x) // works

  println(x + x*y) // failed before, first with type error, after that was fixed with "orphan poly parameter CI".
}
