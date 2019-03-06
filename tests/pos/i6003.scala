object Test {
  opaque type T = String
  object T {
    def unwrap(t: T): String = t
  }

  opaque type U = String
  type W = U
  object U {
    def unwrap(w: W): String = w: U
  }
}