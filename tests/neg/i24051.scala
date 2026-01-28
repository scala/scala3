package example {
  package types {
    opaque type OpaqueType[A] = Unit
    object OpaqueType {
      def apply[A]: OpaqueType[A] = ()
    }
  }

  object exports {
    export example.types.OpaqueType
  }

  import exports.*

  def test[A](a: A)(using ev: A =:= Unit) = a
  val proof = test(OpaqueType[String])  // error - Should fail: OpaqueType[String] should not equal Unit
}
