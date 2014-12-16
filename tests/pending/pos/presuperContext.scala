class A {
  class C extends AnyRef {
// TODO NEEDS MANUAL CHANGE (early initializers)
// BEGIN copied early initializers
val x: A = this
// END copied early initializers
}
}

class B(x: Int)

class D {
  class C(x: Int) extends B({val test: D = this; x}) {
    def this() = {
      this({val test: D = this; 1})
    }
  }
}
