abstract class A {
  def show: String
}
class B extends A {
  def show = "B"
}
class C extends A {
  def show = "C"
}
object M {
  def f given B, C : String = {
    implied a for A = the[B]
    the[A].show
  }
}
object Test extends App {
  implied b for B
  implied c for C
  println(M.f)
}
