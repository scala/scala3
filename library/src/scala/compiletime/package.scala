package scala

package object compiletime {

  /** Use this method when you have a type, do not have a value for it but want to
   *  pattern match on it. For example, given a type `Tup <: Tuple`, one can
   *  pattern-match on it as follows:
   *  ```
   *  inline erasedValue[Tup] match {
   *    case _: EmptyTuple => ...
   *    case _: h *: t => ...
   *  }
   *  ```
   *  This value can only be used in an inline match and the value cannot be used in
   *  the branches.
   */
  erased def erasedValue[T]: T = ???

  /** Used as the initializer of a mutable class or object field, like this:
   *
   *    var x: T = uninitialized
   *
   *  This signifies that the field is not initialized on its own. It is still initialized
   *  as part of the bulk initialization of the object it belongs to, which assigns zero
   *  values such as `null`, `0`, `0.0`, `false` to all object fields.
   */
  erased def uninitialized[T]: T = ???

  /** The error method is used to produce user-defined compile errors during inline expansion.
   *  If an inline expansion results in a call error(msgStr) the compiler produces an error message containing the given msgStr.
   *
   *  ```scala
   *  error("My error message")
   *  ```
   *  or
   *  ```scala
   *  inline def errorOnThisCode(inline x: Any) =
   *    error("My error of this code: " + codeOf(x))
   *  ```
   */
  inline def error(inline msg: String): Nothing = ???

  /** Returns the string representation of argument code:
   *
   *  ```scala
   *  inline def logged(inline p1: Any) =
   *    ("code: " + codeOf(p1), p1)
   *
   *  logged(identity("foo"))
   *  // above is equivalent to:
   *  // ("code: scala.Predef.identity("foo")", identity("foo"))
   *  ```
   *
   *  The formatting of the code is not stable across version of the compiler.
   *
   * @note only `inline` arguments will be displayed as "code".
   *       Other values may display unintutively.
   */
  transparent inline def codeOf(arg: Any): String =
    // implemented in dotty.tools.dotc.typer.Inliner.Intrinsics
    error("Compiler bug: `codeOf` was not evaluated by the compiler")

  /** Checks at compiletime that the provided values is a constant after
   *  inlining and constant folding.
   *
   *  Usage:
   *  ```scala
   *  inline def twice(inline n: Int): Int =
   *    requireConst(n) // compile-time assertion that the parameter `n` is a constant
   *    n + n
   *
   *  twice(1)
   *  val m: Int = ...
   *  twice(m) // error: expected a constant value but found: m
   *  ```
   */
  inline def requireConst(inline x: Boolean | Byte | Short | Int | Long | Float | Double | Char | String): Unit =
    // implemented in dotty.tools.dotc.typer.Inliner
    error("Compiler bug: `requireConst` was not evaluated by the compiler")

  /** Same as `constValue` but returns a `None` if a constant value
   *  cannot be constructed from the provided type. Otherwise returns
   *  that value wrapped in `Some`.
   */
  inline def constValueOpt[T]: Option[T] =
    // implemented in dotty.tools.dotc.typer.Inliner
    error("Compiler bug: `constValueOpt` was not evaluated by the compiler")

  /** Given a constant, singleton type `T`, convert it to a value
   *  of the same singleton type. For example: `assert(constValue[1] == 1)`.
   */
  inline def constValue[T]: T =
    // implemented in dotty.tools.dotc.typer.Inliner
    error("Compiler bug: `constValue` was not evaluated by the compiler")

  /**
   * Use this type to widen a self-type to a tuple. E.g.
   * ```
   * val x: (1, 3) = (1, 3)
   * val y: Widen[x.type] = x
   * ```
   */
  type Widen[Tup <: Tuple] <: Tuple = Tup match {
    case EmptyTuple => EmptyTuple
    case h *: t => h *: t
  }

  /** Given a tuple type `(X1, ..., Xn)`, returns a tuple value
   *  `(constValue[X1], ..., constValue[Xn])`.
   */
  inline def constValueTuple[T <: Tuple]: Widen[T]=
    val res =
      inline erasedValue[T] match
        case _: EmptyTuple => EmptyTuple
        case _: (t *: ts) => constValue[t] *: constValueTuple[ts]
      end match
    res.asInstanceOf[Widen[T]]
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
  transparent inline def summonFrom[T](f: Nothing => T): T =
    error("Compiler bug: `summonFrom` was not evaluated by the compiler")

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
   *  a Tuple.
   *
   *  @tparam T the tuple containing the types of the values to be summoned
   *  @return the given values typed as elements of the tuple
   */
  inline def summonAll[T <: Tuple]: Widen[T] =
    val res =
      inline erasedValue[T] match
        case _: EmptyTuple => EmptyTuple
        case _: (t *: ts) => summonInline[t] *: summonAll[ts]
      end match
    res.asInstanceOf[Widen[T]]
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
