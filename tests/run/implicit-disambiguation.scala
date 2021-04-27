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
  def f(using B, C): String = {
    given a: A = summon[B]
    summon[A].show
  }
}
object Test extends App {
  given b: B()
  given c: C()
  println(M.f)
}
