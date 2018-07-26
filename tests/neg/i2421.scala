transparent object Foo // OK (error would be detected later, in PostTyper)
transparent class Bar // error: modifier(s) `transparent' incompatible with type definition
transparent abstract class Baz // error: modifier(s) `transparent' incompatible with type definition
transparent trait Qux // error: modifier(s) `transparent' incompatible with type definition

object Quux {
  transparent type T // error: modifier(s) `transparent' incompatible with type definition
  transparent var x: Int = 42 // error: modifier(s) `transparent' incompatible with var definition
  transparent lazy val y: Int = 43 // error: modifier(s) `transparent' incompatible with lazy val definition
}
