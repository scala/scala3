package scala.runtime.stdLibPatches

import scala.language.experimental.captureChecking

import scala.annotation.experimental
import scala.annotation.internal.RuntimeChecked

object Predef:
  import compiletime.summonFrom

  transparent inline def assert(inline assertion: Boolean, inline message: => Any): Unit =
    if !assertion then scala.runtime.Scala3RunTime.assertFailed(message)

  transparent inline def assert(inline assertion: Boolean): Unit =
    if !assertion then scala.runtime.Scala3RunTime.assertFailed()

  /**
   * Retrieve the single value of a type with a unique inhabitant.
   *
   * @example {{{
   * object Foo
   * val foo = valueOf[Foo.type]
   * // foo is Foo.type = Foo
   *
   * val bar = valueOf[23]
   * // bar is 23.type = 23
   * }}}
   * @group utilities
   */
  inline def valueOf[T]: T = summonFrom {
    case ev: ValueOf[T] => ev.value
  }

  /** Summon a given value of type `T`. Usually, the argument is not passed explicitly.
   *
   *  @tparam T the type of the value to be summoned
   *  @return the given value typed: the provided type parameter
   */
  transparent inline def summon[T](using x: T): x.type = x

  // Extension methods for working with explicit nulls

  /** Strips away the nullability from a value. Note that `.nn` performs a checked cast,
   *  so if invoked on a `null` value it will throw an `NullPointerException`.
   *  @example {{{
   *  val s1: String | Null = "hello"
   *  val s2: String = s1.nn
   *
   *  val s3: String | Null = null
   *  val s4: String = s3.nn // throw NullPointerException
   *  }}}
   */
  extension [T](x: T | Null) inline def nn: x.type & T =
    if x.asInstanceOf[Any] == null then scala.runtime.Scala3RunTime.nnFail()
    x.asInstanceOf[x.type & T]

  extension (inline x: AnyRef | Null)
    /** Enables an expression of type `T|Null`, where `T` is a subtype of `AnyRef`, to be checked for `null`
     *  using `eq` rather than only `==`. This is needed because `Null` no longer has
     *  `eq` or `ne` methods, only `==` and `!=` inherited from `Any`. */
    inline infix def eq(inline y: AnyRef | Null): Boolean =
      x.asInstanceOf[AnyRef] eq y.asInstanceOf[AnyRef]
    /** Enables an expression of type `T|Null`, where `T` is a subtype of `AnyRef`, to be checked for `null`
     *  using `ne` rather than only `!=`. This is needed because `Null` no longer has
     *  `eq` or `ne` methods, only `==` and `!=` inherited from `Any`. */
    inline infix def ne(inline y: AnyRef | Null): Boolean =
      !(x eq y)

  extension (opt: Option.type)
    @experimental
    inline def fromNullable[T](t: T | Null): Option[T] = Option(t).asInstanceOf[Option[T]]

  /** A type supporting Self-based type classes.
   *
   *    A is TC
   *
   *  expands to
   *
   *    TC { type Self = A }
   *
   *  which is what is needed for a context bound `[A: TC]`.
   */
  @experimental
  infix type is[A <: AnyKind, B <: Any{type Self <: AnyKind}] = B { type Self = A }

  extension [T](x: T)
    /**Asserts that a term should be exempt from static checks that can be reliably checked at runtime.
     * @example {{{
     * val xs: Option[Int] = Option(1)
     * xs.runtimeChecked match
     *    case Some(x) => x // `Some(_)` can be checked at runtime, so no warning
     * }}}
     * @example {{{
     * val xs: List[Int] = List(1,2,3)
     * val y :: ys = xs.runtimeChecked // `_ :: _` can be checked at runtime, so no warning
     * }}}
     */
    inline def runtimeChecked: x.type @RuntimeChecked = x: @RuntimeChecked

end Predef
