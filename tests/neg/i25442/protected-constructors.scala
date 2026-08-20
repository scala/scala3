package protectedCtors

class A protected (x: Int)

// Protected constructor in super call arguments (transitive parent)
class B(a: A) extends A(a.hashCode)
class C extends B(new A(1)) // error

class D protected (x: Any)
class E extends D(new D(1)) // error

// Mixed visibility constructors
class F protected (x: Int) {
  def this() = this(0) // public secondary
}
class G(f: F) extends F(f.hashCode)
class H extends G(new F()) // ok: public secondary in super args
class J extends G(new F(1)) // error: protected primary in super args

// new in primary constructor body
class K extends A(42) {
  val k = new A(1) // error
}

// Lambda in super call arguments
class M(f: () => A) extends A(1)
class N extends M(() => new A(2)) // error
