package scala.compiletime

import scala.annotation.infix

package object ops {
  @infix type ==[X <: AnyVal, Y <: AnyVal] <: Boolean
  @infix type !=[X <: AnyVal, Y <: AnyVal] <: Boolean

  @infix type +[X <: Int | String, Y <: Int | String] = (X, Y) match {
    case (Int, Int) => int.+[X, Y]
    case (String, String) => string.+[X, Y]
    case (String, Int) => string.+[X, ToString[Y]]
    case (Int, String) => string.+[ToString[X], Y]
  }

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

  type ![X <: Boolean] <: Boolean
  @infix type ^[X <: Boolean, Y <: Boolean] <: Boolean
  @infix type &&[X <: Boolean, Y <: Boolean] <: Boolean
  @infix type ||[X <: Boolean, Y <: Boolean] <: Boolean

  private object string {
    type +[X <: String, Y <: String] <: String
  }

  private object int {
    type +[X <: Int, Y <: Int] <: Int
  }
}
