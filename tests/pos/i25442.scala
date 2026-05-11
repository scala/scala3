package protectedCtorsPos

class A protected (x: Int)

// Super calls are allowed
class B extends A(42)

// Inner class extending protected parent
class C extends A(42) {
  class Inner extends A(1)
}

// Mixed visibility: public secondary constructor is accessible
class D protected (x: Int) {
  def this() = this(0)
}
class E(d: D) extends D(d.hashCode)
class F extends E(new D())
