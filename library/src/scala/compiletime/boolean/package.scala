package scala.compiletime

package object boolean {
  type ![X <: Boolean] <: Boolean
  type ^[X <: Boolean, Y <: Boolean] <: Boolean
  type &&[X <: Boolean, Y <: Boolean] <: Boolean
  type ||[X <: Boolean, Y <: Boolean] <: Boolean
}
