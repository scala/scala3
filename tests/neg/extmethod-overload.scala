object Test {
  extension a of (x: Int) with
    def |+| (y: Int) = x + y

  extension b of (x: Int) with {
    def |+| (y: String) = x + y.length
  }
  assert((1 |+| 2) == 3)  // error ambiguous

  locally {
    import b.|+|
    assert((1 |+| "2") == 2)  // OK
  }
}