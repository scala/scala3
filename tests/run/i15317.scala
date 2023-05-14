class Outer {
  val outer = 1
  object Inner {
    def forwarder: Int = inlined
    transparent inline def inlined: Int = outer
  }
  object Inner2 {
    def forwarder: Int = inlined
    inline def inlined: Int = outer
  }
}

@main def Test =
  assert((new Outer).Inner.forwarder == 1)
  assert((new Outer).Inner2.forwarder == 1)
