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

object SideEffects:
  val sideEffects = scala.collection.mutable.ListBuffer.empty[String]

trait E:
  final val a: 7 =
    SideEffects.sideEffects += "E.a"
    7
  final val b =
    SideEffects.sideEffects += "E.b"
    8
end E

class F extends E:
  final val c: 9 =
    SideEffects.sideEffects += "F.c"
    9
  final val d =
    SideEffects.sideEffects += "F.d"
    10
end F

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

  val f: F = new F
  assert(SideEffects.sideEffects.toList == List("E.a", "E.b", "F.c", "F.d"))
  assert(f.a == 7)
  assert(f.b == 8)
  assert(f.c == 9)
  assert(f.d == 10)

  assert(classOf[B].getDeclaredMethods.size == 2)
  assert(classOf[B].getDeclaredFields.isEmpty)

  assert(classOf[C].getDeclaredMethods.size == 2)
  assert(classOf[C].getDeclaredFields.size == 1) // x, but not y

  assert(classOf[D].getDeclaredMethods.isEmpty)
  assert(classOf[D].getFields.isEmpty)

  assert(classOf[E].getDeclaredMethods.size == 5)
  assert(classOf[E].getDeclaredFields.isEmpty)

  assert(classOf[F].getDeclaredMethods.size == 2)
  assert(classOf[F].getDeclaredFields.isEmpty)
