trait P[A]{
  def a[T]: A
}
class C extends P[Int]{ // error: class C needs to be abstract
  def a = 1
}
object O{
  def main(args: Array[String]) = {
    val p: P[Int] = new C
    println(p.a)
  }
}