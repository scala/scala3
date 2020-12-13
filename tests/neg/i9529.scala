given Int = { // should have been `given AnyRef {`
  extension (n: Int) // error
    def plus(m: Int): Int = n + m
}
