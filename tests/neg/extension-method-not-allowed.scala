object Test {
  extension [T] (t: T) {
    extension (c: T) def f: T = ??? // error : No extension method allowed here
  }
}