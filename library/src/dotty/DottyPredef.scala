package dotty

object DottyPredef {
  import compiletime.summonFrom

  inline final def assert(inline assertion: Boolean, inline message: => Any): Unit = {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed: " + message)
  }

  transparent inline final def assert(inline assertion: Boolean): Unit = {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed")
  }

  inline final def implicitly[T](implicit ev: T): T = ev

  /** Used to mark code blocks as being expressions, instead of being taken as part of anonymous classes and the like.
   *  This is just a different name for [[identity]].
   *
   *  @example Separating code blocks from `new`:
   *           {{{
   *             val x = new AnyRef
   *             {
   *               val y = ...
   *               println(y)
   *             }
   *             // the { ... } block is seen as the body of an anonymous class
   *
   *             val x = new AnyRef
   *
   *             {
   *               val y = ...
   *               println(y)
   *             }
   *             // an empty line is a brittle "fix"
   *
   *             val x = new AnyRef
   *             locally {
   *               val y = ...
   *               println(y)
   *             }
   *             // locally guards the block and helps communicate intent
   *           }}}
   *  @group utilities
   */
  inline def locally[T](inline body: T): T = body

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
  extension [T](x: T | Null) def nn: x.type & T =
    if (x == null) throw new NullPointerException("tried to cast away nullability, but value is null")
    else x.asInstanceOf[x.type & T]
}
