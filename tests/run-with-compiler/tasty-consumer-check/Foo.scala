package pack
trait Foo {
  type F = List
  type G[X] = List[X]
  type I <: Int
  type J[X] >: String <: X
}
