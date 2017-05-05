class A {
  def compareTo(o: Any): Int = 0
}
class B extends A with Comparable[B] {
  def compareTo(b: B): Int = 0 // error
}
object C {
  def main(args: Array[String]): Unit = {
    println(new B().compareTo(new Object()))
  }
}
