rewrite object Foo // OK (error would be detected later, in PostTyper)
rewrite class Bar // error: modifier(s) `rewrite' incompatible with type definition
rewrite abstract class Baz // error: modifier(s) `rewrite' incompatible with type definition
rewrite trait Qux // error: modifier(s) `rewrite' incompatible with type definition

object Quux {
  rewrite type T // error: modifier(s) `rewrite' incompatible with type definition
  rewrite var x: Int = 42 // error: modifier(s) `rewrite' incompatible with var definition
  rewrite lazy val y: Int = 43 // error: modifier(s) `rewrite' incompatible with lazy val definition
}
