object Test {
  extension [T] (t: T) {
    def f[U](u: U): T = ??? // error: extension method cannot have type parameters here, all type parameters go after `extension`
  }
}