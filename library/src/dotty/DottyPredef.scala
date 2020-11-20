package dotty

object DottyPredef {
  import compiletime.summonFrom

  inline final def assert(inline assertion: Boolean, inline message: => Any): Unit = {
    if (!assertion)
      scala.runtime.Scala3RunTime.assertFailed(message)
  }

  transparent inline final def assert(inline assertion: Boolean): Unit = {
    if (!assertion)
      scala.runtime.Scala3RunTime.assertFailed()
  }

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
   *  @return the given value typed as the provided type parameter
   */
  inline def summon[T](using x: T): x.type = x

  // Extension methods for working with explicit nulls

  /** Strips away the nullability from a value.
   *  e.g.
   *    val s1: String|Null = "hello"
   *    val s: String = s1.nn
   *
   *  Note that `.nn` performs a checked cast, so if invoked on a null value it'll throw an NPE.
   */
  extension [T](x: T | Null) inline def nn: x.type & T =
    scala.runtime.Scala3RunTime.nn(x)
}
