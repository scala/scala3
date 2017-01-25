class A { { val x = this } }
class B(x: Int) {
  class C(x: Int)
      extends B({
        val test = this
        x
      }) {
    def this() = {
      this({
        1
      })
    }
  }
}

// Minimized version
class D(x: Int) {
  class E(x: Int) extends D({val test = D.this; x})
}

