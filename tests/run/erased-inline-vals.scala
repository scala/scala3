// scalajs: --skip

abstract class A:
  def x: Int
  val y: Int

class B extends A:
  inline def x: Int = 1
  inline val y = 2

class C extends A:
  final val x: Int = 3
  final val y = 4

class D:
  inline def x: Int = 5
  inline val y = 6

@main def Test =
  val b: B = new B
  assert(b.x == 1)
  assert(b.y == 2)

  val a: A = b
  assert(a.x == 1)
  assert(a.y == 2)

  val c: C = new C
  assert(c.x == 3)
  assert(c.y == 4)

  val a2: A = c
  assert(a2.x == 3)
  assert(a2.y == 4)

  val d: D = new D
  assert(d.x == 5)
  assert(d.y == 6)


  assert(classOf[B].getDeclaredMethods.size == 2)
  assert(classOf[B].getDeclaredFields.isEmpty)

  assert(classOf[C].getDeclaredMethods.size == 2)
  assert(classOf[C].getDeclaredFields.isEmpty)

  assert(classOf[D].getDeclaredMethods.isEmpty)
  assert(classOf[D].getFields.isEmpty)
