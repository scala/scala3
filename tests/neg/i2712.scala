object Foo {
  lazy var x: Int = 42 // error: modifier(s) `lazy' incompatible with var definition
  lazy def y: Int = 42 // error: modifier(s) `lazy' incompatible with def definition
}

lazy class Bar // error: modifier(s) `lazy' incompatible with type definition
lazy abstract class Baz // error: modifier(s) `lazy abstract' incompatible with type definition
lazy trait Qux // error: modifier(s) `lazy' not allowed for trait

object Quux {
  lazy type T // error: modifier(s) `lazy' incompatible with type definition
}
