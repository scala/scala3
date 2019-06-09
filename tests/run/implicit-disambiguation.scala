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
    delegate a for A = the[B]
    the[A].show
  }
}
object Test extends App {
  delegate b for B
  delegate c for C
  println(M.f)
}
