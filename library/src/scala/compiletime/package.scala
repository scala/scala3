package scala

import scala.quoted._

package object compiletime {

  erased def erasedValue[T]: T = ???

  /** The error method is used to produce user-defined compile errors during inline expansion.
   *  If an inline expansion results in a call error(msgStr) the compiler produces an error message containing the given msgStr.
   *
   *  ```scala
   *  error("My error message")
   *  ```
   *  or
   *  ```scala
   *  error(code"My error of this code: ${println("foo")}")
   *  ```
   */
  inline def error(inline msg: String): Nothing = ???

  /** Returns the string representation of interpolated elaborated code:
   *
   *  ```scala
   *  inline def logged(p1: => Any) = {
   *    val c = code"code: $p1"
   *    val res = p1
   *    (c, p1)
   *  }
   *  logged(identity("foo"))
   *  // above is equivalent to:
   *  // ("code: scala.Predef.identity("foo")", identity("foo"))
   *  ```
   *
   * @note only by-name arguments will be displayed as "code".
   *       Other values may display unintutively.
   */
  transparent inline def (inline self: StringContext) code (inline args: Any*): String =
    ${ dotty.internal.CompileTimeMacros.codeExpr('self, 'args) }

  inline def constValueOpt[T]: Option[T] = ???

  inline def constValue[T]: T = ???

  inline def constValueTuple[T <: Tuple]: Tuple.Widen[T]=
    val res =
      inline erasedValue[T] match
        case _: EmptyTuple => EmptyTuple
        case _: (t *: ts) => constValue[t] *: constValueTuple[ts]
      end match
    res.asInstanceOf[Tuple.Widen[T]]
  end constValueTuple

  /** Summons first given matching one of the listed cases. E.g. in
   *
   *      given B { ... }
   *
   *      summonFrom {
   *        case given A => 1
   *        case given B => 2
   *        case given C => 3
   *        case _ => 4
   *      }
   *
   *  the returned value would be `2`.
   */
  transparent inline def summonFrom[T](f: Nothing => T): T = ???


  /** Summon a given value of type `T`. Usually, the argument is not passed explicitly.
   *  The summoning is delayed until the call has been fully inlined.
   *
   *  @tparam T the type of the value to be summoned
   *  @return the given value typed as the provided type parameter
   */
  transparent inline def summonInline[T]: T = summonFrom {
    case t: T => t
  }

  /** Given a tuple T, summons each of its member types and returns them in
   *  a List.
   *
   *  @tparam T the tuple containing the types of the values to be summoned
   *  @return the given values typed as elements of the tuple
   */
  inline def summonAll[T <: Tuple]: Tuple.Widen[T] =
    val res =
      inline erasedValue[T] match
        case _: EmptyTuple => EmptyTuple
        case _: (t *: ts) => summonInline[t] *: summonAll[ts]
      end match
    res.asInstanceOf[Tuple.Widen[T]]
  end summonAll

  /** Succesor of a natural number where zero is the type 0 and successors are reduced as if the definition was
   *
   *      type S[N <: Int] <: Int = N match {
   *        case 0 => 1
   *        case 1 => 2
   *        case 2 => 3
   *        ...
   *        case 2147483646 => 2147483647
   *      }
   */
  type S[N <: Int] <: Int

  /** Assertion that an argument is by-name. Used for nullability checking. */
  def byName[T](x: => T): T = x
}
