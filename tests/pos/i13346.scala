object Outer_Typed {
  object Inner {
    class ClassUntyped(x: Int)
    class ClassTyped[T](x: T) // notice type parameter

    // ok
    val _ = new ClassUntyped(42)
    val _ = new ClassTyped("42")
    val _ = ClassUntyped(42)
    val _ = ClassTyped("42")
  }

  export Inner._

  val _ = new ClassUntyped(42)
  val _ = new ClassTyped("42")
  val _ = ClassUntyped(42)
  val _ = ClassTyped("42") // error: Not found: ClassTyped
}
