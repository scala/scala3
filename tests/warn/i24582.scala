// Test for issue #24582: Spurious shadowing warning for type parameters
//> using options -Wshadow:type-parameter-shadow

import scala.compiletime.ops.int.+

// Should NOT warn - only + was imported, not S
type Foo[S] = S

// Various test cases for import shadowing behavior
object TestImportShadowing:
  object MyModule:
    type A = Int
    type B = String
    val x = 1

  // Should NOT warn - specific import of A only
  import MyModule.A
  type Test1[B] = B

  // Should warn - A was explicitly imported and is being shadowed
  type Test2[A] = A // warn

  object WildcardTest:
    import MyModule.*
    // Should warn - wildcard import brings in B
    type Test3[B] = B // warn

  object RenamedTest:
    import MyModule.{A => RenamedA}
    // Should NOT warn - A is imported but renamed to RenamedA
    type Test4[A] = A

  object MultipleSelectorTest:
    import MyModule.{A, x}
    // Should warn - A was explicitly imported
    type Test5[A] = A // warn
    // Should NOT warn - B was not imported
    type Test6[B] = B

end TestImportShadowing
