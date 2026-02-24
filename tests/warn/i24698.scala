//> using options -Wunused:all

trait TC[X] {
  def notTrivial: Int
}

class C[T: TC]() { // warn implicit TC used only for new instance of owner
  def mod: C[T] = new C
}

class D(val i: Int):
  def this(s: String) = this(s.toInt) // not new

class E[T](tc: TC[T]): // warn
  def mod: E[T] = new E(tc)

class F[T: TC](i: Int): // warn // warn
  def mod: F[T] = new F(i)

class G(i: Int): // warn
  class Inner(i: Int):
    def g = new G(i)
