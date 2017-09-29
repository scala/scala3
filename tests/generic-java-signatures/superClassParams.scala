class A[T]
class B extends A[Int]
class C extends A[String]
class D extends A[java.util.List[Int]]
class E extends A[java.util.List[String]]
object Test {
  def main(args: Array[String]): Unit = {
    println("B: " + classOf[B].getGenericSuperclass.getTypeName)
    println("C: " + classOf[C].getGenericSuperclass.getTypeName)
    println("D: " + classOf[B].getGenericSuperclass.getTypeName)
    println("E: " + classOf[C].getGenericSuperclass.getTypeName)
  }
}