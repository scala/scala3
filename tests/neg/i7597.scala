object Test extends App {
  def foo[S <: String]: String => Int =
    new (String => Int) { def apply(s: S): Int = 0 } // error

  trait Fn[A, B] {
    def apply(x: A): B
  }

  class C[S <: String] extends Fn[String, Int] {
    def apply(s: S): Int = 0 // error
  }

  foo("")
}
