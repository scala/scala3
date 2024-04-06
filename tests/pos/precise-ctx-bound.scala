//> using options -language:experimental.modularity -source future
object Test:

  class Wrap[T](x: T)

  def f0[T](x: T): Wrap[T] = Wrap(x)
  val x0 = f0(1)
  val _: Wrap[Int] = x0

  def f1[T: Precise](x: T): Wrap[T] = Wrap(x)
  def l = "hello".length
  val x1 = Wrap(l)
  val _: Wrap[Int] = x1

  def f2[T](x: T)(using Precise { type Self = T}): Wrap[T] = Wrap(x)
  val x2 = f2(1)
  val _: Wrap[1] = x2

  def f3[T: Precise](x: T): Wrap[T] = Wrap(x)
  val x3 = f3(identity(1))
  val _: Wrap[1] = x3
  val x3a = f3(1 + 2)
  val _: Wrap[3] = x3a

  def f4[T](x: T)(using T is Precise): Wrap[T] = Wrap(x)
  val x4 = f4(1)
  val _: Wrap[1] = x4
  val x4a = f4(1 + 2)
  val _: Wrap[3] = x4a
  val y4 = f4(if ??? then 1 else 2)
  val _: Wrap[1 | 2] = y4
  val z4 = f4(if ??? then B() else C())
  val _: Wrap[B | C] = z4
  trait A
  class B extends A
  class C extends A

  class C0[T](x: T):
    def fld: T = x
  val y0 = C0("hi")
  val _: String = y0.fld

  class C2[T](x: T)(using T is Precise):
    def fld: T = x
  val y2 = C2(identity("hi"))
  val _: "hi" = y2.fld

  class C3[T: Precise](x: T):
    def fld: T = x
  val y3 = C3("hi")
  val _: "hi" = y3.fld
