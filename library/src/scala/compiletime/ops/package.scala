package scala.compiletime

package object ops {
  type ==[X <: AnyVal, Y <: AnyVal] <: Boolean
  type !=[X <: AnyVal, Y <: AnyVal] <: Boolean

  type +[X <: Int, Y <: Int] <: Int
  type -[X <: Int, Y <: Int] <: Int
  type *[X <: Int, Y <: Int] <: Int
  type /[X <: Int, Y <: Int] <: Int
  type %[X <: Int, Y <: Int] <: Int

  type <[X <: Int, Y <: Int] <: Boolean
  type >[X <: Int, Y <: Int] <: Boolean
  type >=[X <: Int, Y <: Int] <: Boolean
  type <=[X <: Int, Y <: Int] <: Boolean

  type Abs[X <: Int] <: Int
  type Negate[X <: Int] <: Int
  type Min[X <: Int, Y <: Int] <: Int
  type Max[X <: Int, Y <: Int] <: Int

  type ![X <: Boolean] <: Boolean
  type ^[X <: Boolean, Y <: Boolean] <: Boolean
  type &&[X <: Boolean, Y <: Boolean] <: Boolean
  type ||[X <: Boolean, Y <: Boolean] <: Boolean
}
