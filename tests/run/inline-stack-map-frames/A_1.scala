//> using options -opt -opt-inline:**
class A {
  @noinline final def b: B = null
  @inline final def a: A = b
}

class B extends A

class D extends A
