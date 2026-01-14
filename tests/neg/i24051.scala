// Test for issue #24051: Opaque type export bug
// Exported opaque types should not leak their underlying type

// Test 1: Opaque type with type parameter, exported, underlying type is Unit
package example1 {
  package types {
    opaque type OpaqueType[A] = Unit
    object OpaqueType {
      def apply[A]: OpaqueType[A] = ()
    }
  }

  object exports {
    export example1.types.OpaqueType
  }

  import exports.*

  def test[A](a: A)(using ev: A =:= Unit) = a

  // Using the exported type should fail - OpaqueType[String] is NOT Unit from outside types
  val proof = test(OpaqueType[String])  // error

  // For comparison, using the direct type also fails correctly
  val proof2 = test(types.OpaqueType[String])  // error
}

// Test 2: Opaque type with type parameter, exported, underlying type is Int
package example2 {
  package types {
    opaque type OpaqueInt[A] = Int
    object OpaqueInt {
      def apply[A]: OpaqueInt[A] = 42
    }
  }

  object exports {
    export example2.types.OpaqueInt
  }

  import exports.*

  def test[A](a: A)(using ev: A =:= Int) = a

  // Both should fail - OpaqueInt[String] is NOT Int from outside types
  val proof = test(OpaqueInt[String])  // error
  val proof2 = test(types.OpaqueInt[String])  // error
}

// Test 3: Simple opaque type without type parameter
package example3 {
  package lib {
    opaque type MyString = String
    object MyString {
      def make(s: String): MyString = s
    }
  }

  object user {
    // This should fail - MyString is NOT String from outside lib
    val x: String = lib.MyString.make("hello")  // error
  }
}

// Test 4: Opaque type exported and used with explicit type annotation
package example4 {
  package types {
    opaque type Wrapper[A] = A
    object Wrapper {
      def wrap[A](a: A): Wrapper[A] = a
    }
  }

  object exports {
    export example4.types.Wrapper
  }

  object user {
    import exports.*

    // This should fail - cannot assign Wrapper[Int] to Int
    val x: Int = Wrapper.wrap(42)  // error

    // Direct access should also fail
    val y: Int = types.Wrapper.wrap(42)  // error
  }
}
