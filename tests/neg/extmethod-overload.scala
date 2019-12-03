object Test {
  given a: (x: Int) extended with
    def |+| (y: Int) = x + y

  given b: (x: Int) extended with {
    def |+| (y: String) = x + y.length
  }
  assert((1 |+| 2) == 3)  // error ambiguous

  locally {
    import b.|+|
    assert((1 |+| "2") == 2)  // OK
  }
}