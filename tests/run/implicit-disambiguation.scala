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
    given a as A = the[B]
    the[A].show
  }
}
object Test extends App {
  given b as B
  given c as C
  println(M.f)
}
