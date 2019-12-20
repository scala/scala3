package scala.compiletime

import scala.annotation.infix

package object ops {
  @infix type ==[X <: AnyVal, Y <: AnyVal] <: Boolean
  @infix type !=[X <: AnyVal, Y <: AnyVal] <: Boolean

  object string {
    @infix type +[X <: String, Y <: String] <: String
  }

  object int {
    @infix type +[X <: Int, Y <: Int] <: Int
    @infix type -[X <: Int, Y <: Int] <: Int
    @infix type *[X <: Int, Y <: Int] <: Int
    @infix type /[X <: Int, Y <: Int] <: Int
    @infix type %[X <: Int, Y <: Int] <: Int

    @infix type <[X <: Int, Y <: Int] <: Boolean
    @infix type >[X <: Int, Y <: Int] <: Boolean
    @infix type >=[X <: Int, Y <: Int] <: Boolean
    @infix type <=[X <: Int, Y <: Int] <: Boolean

    type Abs[X <: Int] <: Int
    type Negate[X <: Int] <: Int
    type Min[X <: Int, Y <: Int] <: Int
    type Max[X <: Int, Y <: Int] <: Int
    type ToString[X <: Int] <: String
  }

  object boolean {
    type ![X <: Boolean] <: Boolean
    @infix type ^[X <: Boolean, Y <: Boolean] <: Boolean
    @infix type &&[X <: Boolean, Y <: Boolean] <: Boolean
    @infix type ||[X <: Boolean, Y <: Boolean] <: Boolean
  }
}
