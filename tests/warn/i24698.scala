//> using options -Wunused:params

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

final class Foo(x: Int) { // warn #27449
  def copy: Foo = new Foo(x)
  def also: Foo = Foo(x)
}

class Bar(implicit x: Int) { // warn
  def copy: Bar = new Bar()(using x)
}

class Baz(using x: Int) { // warn
  def copy: Baz = new Baz(using x)
}

class i27449(i: Int): // no warn although apply is trivial forwarder
  def copy = i27449(i)

object i27449:
  def apply(i: Int) = new i27449(i)
