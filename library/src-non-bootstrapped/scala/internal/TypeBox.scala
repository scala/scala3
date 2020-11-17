package scala.internal

/** A type for skolems that are generated during capture conversion. Capture conversion
 *  narrows the type of a tree whose type has wildcard arguments. A typical situation
 *  is a tree `t` of type `C[_ >: L <: U]` and an expected type `C[X]` where `X` is an
 *  instantiatable type variable. To be able to instantiate `X`, we cast the tree to type
 *  `X[$n.CAP]` where `$n` is a fresh skolem type with underlying type `TypeBox[L, U]`.
 */
final abstract class TypeBox[-L <: U, +U] {
  type CAP >: L <: U
}
