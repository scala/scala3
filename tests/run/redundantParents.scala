// scalajs: --skip

trait T1
trait T2 extends T1
trait T3 extends T2
trait T4 extends T3

trait T5

class C1 extends T2
class C2 extends C1 with T4 with T5 with T1 with T2
class C3 extends C1 with T5
class C4 extends C2 with T5

object Test {
  def main(args: Array[String]): Unit = {
    val c1 = (new C1).getClass
    val c2 = (new C2).getClass
    val c3 = (new C3).getClass
    val c4 = (new C4).getClass

    println("C1 super class: " + c1.getSuperclass)
    println("C1 interfaces: " + c1.getInterfaces.toList)
    println("C2 super class: " + c2.getSuperclass)
    println("C2 interfaces: " + c2.getInterfaces.toList)
    println("C3 super class: " + c3.getSuperclass)
    println("C3 interfaces: " + c3.getInterfaces.toList)
    println("C4 super class: " + c4.getSuperclass)
    println("C4 interfaces: " + c4.getInterfaces.toList)
  }
}
