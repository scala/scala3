package scala.internal

final abstract class TypeBox[-L, +U] {
  type CAP >: L <: U
}
