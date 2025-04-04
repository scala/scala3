object Test:
  def main(args: Array[String]): Unit =
    val a = new A
    println(a.m1)
    println(a.m2)

trait TraitA:
  private var u: String = "u"
  def m2: String = u

abstract class ParentA:
  private val x: String = "x"
  private var y: String = "y"
  private lazy val z: String = "z"
  def m1: String =
    val b = new B
    b.m + m2
  private def m2: String = y + z
  private abstract class ParentB:
    def m: String = x
  private class B extends ParentB

class A extends ParentA with TraitA
