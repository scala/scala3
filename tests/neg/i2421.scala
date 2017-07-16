inline object Foo // error: modifier(s) `inline' incompatible with type definition
inline class Bar // error: modifier(s) `inline' incompatible with type definition
inline abstract class Baz // error: modifier(s) `inline' incompatible with type definition
inline trait Qux // error: modifier(s) `inline' incompatible with type definition

object Quux {
  inline type T // error: modifier(s) `inline' incompatible with type definition
  inline var x: Int = 42 // error: modifier(s) `inline' incompatible with var definition
  inline lazy val y: Int = 43 // error: modifier(s) `inline' incompatible with lazy val definition
}
