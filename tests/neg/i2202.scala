class A {
  def compareTo(o: Any): Int = 0
}
class B extends A with Comparable[B] { // error
  def compareTo(b: B): Int = 0
}
object C {
  def main(args: Array[String]): Unit = {
    println(new B().compareTo(new Object()))
  }
}
