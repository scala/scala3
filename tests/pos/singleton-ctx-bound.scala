//> using options -language:experimental.modularity -source future -language:experimental.erasedDefinitions
object Test:

  class Wrap[T](x: T)

  def f0[T](x: T): Wrap[T] = Wrap(x)
  val x0 = f0(1)
  val _: Wrap[Int] = x0

  def f1[T <: Singleton](x: T): Wrap[T] = Wrap(x)
  val x1 = f1(1)
  val _: Wrap[1] = x1

  def f2[T](x: T)(using erased Singleton { type Self = T}): Wrap[T] = Wrap(x)
  val x2 = f2(1)
  val _: Wrap[1] = x2

  def f3[T: Singleton](x: T): Wrap[T] = Wrap(x)
  val x3 = f3(1)
  val _: Wrap[1] = x3

  def f4[T](x: T)(using erased T is Singleton): Wrap[T] = Wrap(x)
  val x4 = f4(1)
  val _: Wrap[1] = x4

  class C0[T](x: T):
    def fld: T = x
  val y0 = C0("hi")
  val _: String = y0.fld

  class C1[T <: Singleton](x: T):
    def fld: T = x
  val y1 = C1("hi")
  val _: "hi" = y1.fld

  class C2[T](x: T)(using erased T is Singleton):
    def fld: T = x
  val y2 = C2("hi")
  val _: "hi" = y2.fld

  class C3[T: Singleton](x: T):
    def fld: T = x
  val y3 = C3("hi")
  val _: "hi" = y3.fld



