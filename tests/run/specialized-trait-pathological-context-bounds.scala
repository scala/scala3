//> using options -language:experimental.specializedTraits

trait A[T, R, Q]

inline trait Trait[T: {Specialized, Numeric}, S <: Object, Q: Numeric, R: Specialized, D: {Numeric, Specialized}](a: Int) extends A[S, Char, T] {
  def bar = "Buna saira"
}

def foo(v: Trait[Int, String, Int, Int, Int]) = v

object Test:
  def main(args: Array[String]): Unit = {
    val a = new Trait[Int, String, Int, Int, Int](5) {}
    assert(foo(a).bar == "Buna saira")
  }

