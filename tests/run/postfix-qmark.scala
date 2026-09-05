//> using options -Yexplicit-nulls
import language.experimental.errorHandling

case class C(elem: Int):
  def ? (y: Int) = C(elem + y)
  def ? = C(elem)

def foo(x: C, y: C) = ()

@main def Test =
  val c = C(1)
  val x = c ? 1
  assert(x.elem == 2)
  val y = c ?
  identity(1)
  assert(y.elem == 1)
  val z = c
    ? 1
  assert(z.elem == 2)
  foo(c?, c?)
  val xx = c?
  println("done")
