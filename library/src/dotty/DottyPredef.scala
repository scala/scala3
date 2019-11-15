package dotty

object DottyPredef {
  import compiletime.summonFrom

  inline final def assert(assertion: => Boolean, message: => Any): Unit = {
    if (!assertion)
      assertFail(message)
  }

  inline final def assert(assertion: => Boolean) <: Unit = {
    if (!assertion)
      assertFail()
  }

  def assertFail(): Nothing = throw new java.lang.AssertionError("assertion failed")
  def assertFail(message: => Any): Nothing = throw new java.lang.AssertionError("assertion failed: " + message)

  inline final def implicitly[T](implicit ev: T): T = ev

  inline def locally[T](body: => T): T = body

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

  inline def summon[T](given x: T): x.type = x

  // Extension methods for working with explicit nulls

  /** Strips away the nullability from a value.
   *  e.g.
   *    val s1: String|Null = "hello"
   *    val s: String = s1.nn
   *
   *  Note that `.nn` performs a checked cast, so if invoked on a null value it'll throw an NPE.
   */
  def[T] (x: T|Null) nn: T =
    if (x == null) throw new NullPointerException("tried to cast away nullability, but value is null")
    else x.asInstanceOf[T]

  /** Reference equality where the receiver is a nullable union.
   *  Note that if the receiver `r` is a reference type (e.g. `String`), then `r.eq` will invoke the
   *  `eq` method in `AnyRef`.
   */
  def (x: AnyRef|Null) eq(y: AnyRef|Null): Boolean =
    (x == null && y == null) || (x != null && x.eq(y))

  /** Reference disequality where the receiver is a nullable union.
   *  Note that if the receiver `r` is a reference type (e.g. `String`), then `r.ne` will invoke the
   *  `ne` method in `AnyRef`.
   */
  def (x: AnyRef|Null) ne(y: AnyRef|Null): Boolean =
    (x == null && y != null) || (x != null && x.ne(y))

}
