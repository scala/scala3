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
  given b: B with {}
  given c: C with {}
  println(M.f)
}
