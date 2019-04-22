package dotty

object DottyPredef {

  @forceInline final def assert(assertion: => Boolean, message: => Any): Unit = {
    if (!assertion)
      assertFail(message)
  }

  @forceInline final def assert(assertion: => Boolean): Unit = {
    if (!assertion)
      assertFail()
  }

  def assertFail(): Unit = throw new java.lang.AssertionError("assertion failed")
  def assertFail(message: => Any): Unit = throw new java.lang.AssertionError("assertion failed: " + message)

  @forceInline final def implicitly[T](implicit ev: T): T = ev

  @forceInline def locally[T](body: => T): T = body

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
  inline def valueOf[T]: T = implicit match {
    case ev: ValueOf[T] => ev.value
  }

  inline def the[T](implicit x: T): x.type = x
}
