package scala.compiletime

package object int {
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
}
