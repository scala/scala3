import language.experimental.captureChecking
import caps.*

trait Ref[T] { def set(x: T): T }
def test() = {

  def swap[T](x: Ref[T]^)(sep {x} y: Ref[T]^): Unit = ???
  def foo[T](x: Ref[T]^)(sep y: Ref[T]^): Unit =
    swap(x)(y)

  def test1[T](a: Ref[T]^, sep{a} b: Ref[T]^): Unit =
    foo(a)(b)  // ok
    foo(a)(a)  // error
}

def test2(a: Ref[Int]^, sep{a} b: Ref[Boolean]^, sep{a, b} c: Ref[Boolean]^) = {
  type Op = () -> Unit
  def par(f: Op^)(sep{f} g: Op^): Unit = ???

  def writeBoth[A, B](x: Ref[A]^)(sep y: Ref[B]^): Unit =
    par(() => x.set(???))(() => y.set(???))

  writeBoth(a)(b)  // ok
  writeBoth(a)(a)  // error
}
