object Test {
  extension a on (x: Int) {
    def |+| (y: Int) = x + y
  }

  extension b on (x: Int) {
    def |+| (y: String) = x + y.length
  }
  assert((1 |+| 2) == 3)  // error ambiguous

  locally {
    import b.|+|
    assert((1 |+| "2") == 2)  // OK
  }
}