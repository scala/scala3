object Test {
  extension on[T] (t: T) {
    def (c: T).f: T = ??? // error : No extension method allowed here
  }
}