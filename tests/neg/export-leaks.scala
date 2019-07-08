// Check that exports do not leak private types
object Signature {

  private type T

  object O1 {
    private[Signature] def bar: T = ???
  }
  export O1._ // error: non-private method bar refers to private type T

  object O2 {
    private[Signature] val foo: T = ???
  }
  export O2._ // error: non-private method foo refers to private type T
}