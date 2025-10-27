/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc. dba Akka
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala

import scala.language.`2.13`
import scala.language.implicitConversions

import scala.collection.{mutable, immutable, ArrayOps, StringOps}, immutable.WrappedString
import scala.annotation.{elidable, experimental, implicitNotFound, publicInBinary, targetName }, elidable.ASSERTION
import scala.annotation.meta.{ companionClass, companionMethod }
import scala.annotation.internal.{ RuntimeChecked }
import scala.compiletime.summonFrom
import scala.runtime.ScalaRunTime.mapNull

/** The `Predef` object provides definitions that are accessible in all Scala
 *  compilation units without explicit qualification.
 *
 *  === Commonly Used Types ===
 *  Predef provides type aliases for types which are commonly used, such as
 *  the immutable collection types [[scala.collection.immutable.Map]] and
 *  [[scala.collection.immutable.Set]].
 *
 *  === Console Output ===
 *  For basic console output, `Predef` provides convenience methods [[print(x:Any* print]] and [[println(x:Any* println]],
 *  which are aliases of the methods in the object [[scala.Console]].
 *
 *  === Assertions ===
 *  A set of `assert` functions are provided for use as a way to document
 *  and dynamically check invariants in code. Invocations of `assert` can be elided
 *  at compile time by providing the command line option `-Xdisable-assertions`,
 *  which raises `-Xelide-below` above `elidable.ASSERTION`, to the `scalac` command.
 *
 *  Variants of `assert` intended for use with static analysis tools are also
 *  provided: `assume`, `require` and `ensuring`. `require` and `ensuring` are
 *  intended for use as a means of design-by-contract style specification
 *  of pre- and post-conditions on functions, with the intention that these
 *  specifications could be consumed by a static analysis tool. For instance,
 *
 *  {{{
 *  def addNaturals(nats: List[Int]): Int = {
 *    require(nats forall (_ >= 0), "List contains negative numbers")
 *    nats.foldLeft(0)(_ + _)
 *  } ensuring(_ >= 0)
 *  }}}
 *
 *  The declaration of `addNaturals` states that the list of integers passed should
 *  only contain natural numbers (i.e. non-negative), and that the result returned
 *  will also be natural. `require` is distinct from `assert` in that if the
 *  condition fails, then the caller of the function is to blame rather than a
 *  logical error having been made within `addNaturals` itself. `ensuring` is a
 *  form of `assert` that declares the guarantee the function is providing with
 *  regards to its return value.
 *
 *  === Implicit Conversions ===
 *  A number of commonly applied implicit conversions are also defined here, and
 *  in the parent type [[scala.LowPriorityImplicits]]. Implicit conversions
 *  are provided for the "widening" of numeric values, for instance, converting a
 *  Short value to a Long value as required, and to add additional higher-order
 *  functions to Array values. These are described in more detail in the documentation of [[scala.Array]].
 *
 * @groupname utilities Utility Methods
 * @groupprio utilities 10
 *
 * @groupname assertions Assertions
 * @groupprio assertions 20
 * @groupdesc assertions These methods support program verification and runtime correctness.
 *
 * @groupname console-output Console Output
 * @groupprio console-output 30
 * @groupdesc console-output These methods provide output via the console.
 *
 * @groupname aliases Aliases
 * @groupprio aliases 50
 * @groupdesc aliases These aliases bring selected immutable types into scope without any imports.
 *
 * @groupname conversions-string String Conversions
 * @groupprio conversions-string 60
 * @groupdesc conversions-string Conversions from String to StringOps or WrappedString.
 *
 * @groupname implicit-classes-any Implicit Classes
 * @groupprio implicit-classes-any 70
 * @groupdesc implicit-classes-any These implicit classes add useful extension methods to every type.
 *
 * @groupname char-sequence-wrappers CharSequence Wrappers
 * @groupprio char-sequence-wrappers 80
 * @groupdesc char-sequence-wrappers Wrappers that implements CharSequence and were implicit classes.
 *
 * @groupname conversions-java-to-anyval Java to Scala
 * @groupprio conversions-java-to-anyval 90
 * @groupdesc conversions-java-to-anyval Implicit conversion from Java primitive wrapper types to Scala equivalents.
 *
 * @groupname conversions-anyval-to-java Scala to Java
 * @groupprio conversions-anyval-to-java 100
 * @groupdesc conversions-anyval-to-java Implicit conversion from Scala AnyVals to Java primitive wrapper types equivalents.
 *
 * @groupname conversions-array-to-wrapped-array Array to ArraySeq
 * @groupprio conversions-array-to-wrapped-array 110
 * @groupdesc conversions-array-to-wrapped-array Conversions from Arrays to ArraySeqs.
 */
object Predef extends LowPriorityImplicits {
  /**
   * Retrieve the runtime representation of a class type. `classOf[T]` is equivalent to
   * the class literal `T.class` in Java.
   *
   * @example {{{
   * val listClass = classOf[List[_]]
   * // listClass is java.lang.Class[List[_]] = class scala.collection.immutable.List
   *
   * val mapIntString = classOf[Map[Int,String]]
   * // mapIntString is java.lang.Class[Map[Int,String]] = interface scala.collection.immutable.Map
   * }}}
   *
   * @return The runtime [[Class]] representation of type `T`.
   * @group utilities
   */
  def classOf[T]: Class[T] = null.asInstanceOf[Class[T]] // This is a stub method. The actual implementation is filled in by the compiler.

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
  @inline def valueOf[T](implicit vt: ValueOf[T]): T = vt.value

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

  /** The `String` type in Scala has all the methods of the underlying
   *  [[java.lang.String]], of which it is just an alias.
   *
   *  In addition, extension methods in [[scala.collection.StringOps]]
   *  are added implicitly through the conversion [[augmentString]].
   *  @group aliases
   */
  type String        = java.lang.String
  /**  @group aliases */
  type Class[T]      = java.lang.Class[T]

  // miscellaneous -----------------------------------------------------
  scala.`package`                         // to force scala package object to be seen.
  scala.collection.immutable.List         // to force Nil, :: to be seen.

  /**  @group aliases */
  type Function[-A, +B] = Function1[A, B]

  /**  @group aliases */
  type Map[K, +V] = immutable.Map[K, V]
  /**  @group aliases */
  type Set[A]     = immutable.Set[A]
  /**  @group aliases */
  val Map         = immutable.Map
  /**  @group aliases */
  val Set         = immutable.Set

  /**
   * Allows destructuring tuples with the same syntax as constructing them.
   *
   * @example {{{
   * val tup = "foobar" -> 3
   *
   * val c = tup match {
   *   case str -> i => str.charAt(i)
   * }
   * }}}
   * @group aliases
   */
  val ->        = Tuple2

  // Manifest types, companions, and incantations for summoning
  // TODO undeprecated until Scala reflection becomes non-experimental
  // @deprecated("this notion doesn't have a corresponding concept in 2.10, because scala.reflect.runtime.universe.TypeTag can capture arbitrary types. Use type tags instead of manifests, and there will be no need in opt manifests.", "2.10.0")
  type OptManifest[T]   = scala.reflect.OptManifest[T]
  @implicitNotFound(msg = "No Manifest available for ${T}.")
  // TODO undeprecated until Scala reflection becomes non-experimental
  // @deprecated("use `scala.reflect.ClassTag` (to capture erasures) or scala.reflect.runtime.universe.TypeTag (to capture types) or both instead", "2.10.0")
  type Manifest[T]      = scala.reflect.Manifest[T]
  // TODO undeprecated until Scala reflection becomes non-experimental
  // @deprecated("use `scala.reflect.ClassTag` (to capture erasures) or scala.reflect.runtime.universe.TypeTag (to capture types) or both instead", "2.10.0")
  val Manifest          = scala.reflect.Manifest
  // TODO undeprecated until Scala reflection becomes non-experimental
  // @deprecated("this notion doesn't have a corresponding concept in 2.10, because scala.reflect.runtime.universe.TypeTag can capture arbitrary types. Use type tags instead of manifests, and there will be no need in opt manifests.", "2.10.0")
  val NoManifest        = scala.reflect.NoManifest

  // TODO undeprecated until Scala reflection becomes non-experimental
  // @deprecated("use scala.reflect.classTag[T] and scala.reflect.runtime.universe.typeTag[T] instead", "2.10.0")
  def manifest[T](implicit m: Manifest[T]): Manifest[T]          = m
  // TODO undeprecated until Scala reflection becomes non-experimental
  // @deprecated("this notion doesn't have a corresponding concept in 2.10, because scala.reflect.runtime.universe.TypeTag can capture arbitrary types. Use type tags instead of manifests, and there will be no need in opt manifests.", "2.10.0")
  def optManifest[T](implicit m: OptManifest[T]): OptManifest[T] = m

  // Minor variations on identity functions

  /**
   * A method that returns its input value.
   * @tparam A type of the input value x.
   * @param x the value of type `A` to be returned.
   * @return the value `x`.
   * @group utilities */
  @inline def identity[A](x: A): A = x // see `$conforms` for the implicit version

  /** Summon an implicit value of type `T`. Usually, the argument is not passed explicitly.
   *
   *  @tparam T the type of the value to be summoned
   *  @return the implicit value of type `T`
   *  @group utilities
   */
  @inline def implicitly[T](implicit e: T): T = e // TODO: when dependent method types are on by default, give this result type `e.type`, so that inliner has better chance of knowing which method to inline in calls like `implicitly[MatchingStrategy[Option]].zero`

  /** Summon a given value of type `T`. Usually, the argument is not passed explicitly.
   *
   *  @tparam T the type of the value to be summoned
   *  @return the given value typed: the provided type parameter
   */
  transparent inline def summon[T](using x: T): x.type = x

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
  @inline def locally[T](@deprecatedName("x") x: T): T = x

  // ==============================================================================================
  // ========================================= ASSERTIONS =========================================
  // ==============================================================================================

  /* In Scala 3, `assert` are methods that are `transparent` and `inline`.
     In Scala 2, `assert` are methods that are elidable, inlinable by the optimizer
     For scala 2 code to be able to run with the scala 3 library in the classpath
     (following our own compatibility policies), we will need the `assert` methods
     to be available at runtime.
     To achieve this, we keep the Scala 3 signature publicly available.
     We rely on the fact that it is `inline` and will not be visible in the bytecode.
     To add the required Scala 2 ones, we define the `scala2Assert`, we use:
      - `@targetName` to swap the name in the generated code to `assert`
      - `@publicInBinary` to make it available during runtime.
     As such, we would successfully hijack the definitions of `assert` such as:
      - At compile time, we would have the definitions of `assert`
      - At runtime, the definitions of `scala2Assert` as `assert`
    NOTE: Tasty-Reader in Scala 2 will have to learn about this swapping if we are to
    allow loading the full Scala 3 library by it.
    */

  /** Tests an expression, throwing an `AssertionError` if false.
   *  Calls to this method will not be generated if `-Xelide-below`
   *  is greater than `ASSERTION`.
   *
   *  @see [[scala.annotation.elidable elidable]]
   *  @param assertion   the expression to test
   *  @group assertions
   */
  @elidable(ASSERTION) @publicInBinary
  @targetName("assert") private[scala] def scala2Assert(assertion: Boolean): Unit = {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed")
  }

  /** Tests an expression, throwing an `AssertionError` if false.
   *  Calls to this method will not be generated if `-Xelide-below`
   *  is greater than `ASSERTION`.
   *
   *  @see [[scala.annotation.elidable elidable]]
   *  @param assertion   the expression to test
   *  @param message     a String to include in the failure message
   *  @group assertions
   */
  @elidable(ASSERTION) @inline @publicInBinary
  @targetName("assert") private[scala] final def scala2Assert(assertion: Boolean, message: => Any): Unit = {
    if (!assertion)
      throw new java.lang.AssertionError("assertion failed: "+ message)
  }

  transparent inline def assert(inline assertion: Boolean, inline message: => Any): Unit =
    if !assertion then scala.runtime.Scala3RunTime.assertFailed(message)

  transparent inline def assert(inline assertion: Boolean): Unit =
    if !assertion then scala.runtime.Scala3RunTime.assertFailed()

  // ==============================================================================================
  // ======================================== ASSUMPTIONS =========================================
  // ==============================================================================================

  /** Tests an expression, throwing an `AssertionError` if false.
   *  This method differs from assert only in the intent expressed:
   *  assert contains a predicate which needs to be proven, while
   *  assume contains an axiom for a static checker.  Calls to this method
   *  will not be generated if `-Xelide-below` is greater than `ASSERTION`.
   *
   *  @see [[scala.annotation.elidable elidable]]
   *  @param assumption   the expression to test
   *  @group assertions
   */
  @elidable(ASSERTION)
  def assume(assumption: Boolean): Unit = {
    if (!assumption)
      throw new java.lang.AssertionError("assumption failed")
  }

  /** Tests an expression, throwing an `AssertionError` if false.
   *  This method differs from assert only in the intent expressed:
   *  assert contains a predicate which needs to be proven, while
   *  assume contains an axiom for a static checker.  Calls to this method
   *  will not be generated if `-Xelide-below` is greater than `ASSERTION`.
   *
   *  @see [[scala.annotation.elidable elidable]]
   *  @param assumption   the expression to test
   *  @param message      a String to include in the failure message
   *  @group assertions
   */
  @elidable(ASSERTION) @inline
  final def assume(assumption: Boolean, message: => Any): Unit = {
    if (!assumption)
      throw new java.lang.AssertionError("assumption failed: "+ message)
  }

  /** Tests an expression, throwing an `IllegalArgumentException` if false.
   *  This method is similar to `assert`, but blames the caller of the method
   *  for violating the condition.
   *
   *  @param requirement   the expression to test
   *  @group assertions
   */
  def require(requirement: Boolean): Unit = {
    if (!requirement)
      throw new IllegalArgumentException("requirement failed")
  }

  /** Tests an expression, throwing an `IllegalArgumentException` if false.
   *  This method is similar to `assert`, but blames the caller of the method
   *  for violating the condition.
   *
   *  @param requirement   the expression to test
   *  @param message       a String to include in the failure message
   *  @group assertions
   */
  @inline final def require(requirement: Boolean, message: => Any): Unit = {
    if (!requirement)
      throw new IllegalArgumentException("requirement failed: "+ message)
  }

  /** `???` can be used for marking methods that remain to be implemented.
   *  @throws NotImplementedError when `???` is invoked.
   *  @group utilities
   */
  def ??? : Nothing = throw new NotImplementedError

  // implicit classes -----------------------------------------------------

  /** @group implicit-classes-any */
  implicit final class ArrowAssoc[A](private val self: A) extends AnyVal {
    @inline def -> [B](y: B): (A, B) = (self, y)
    @deprecated("Use `->` instead. If you still wish to display it as one character, consider using a font with programming ligatures such as Fira Code.", "2.13.0")
    def →[B](y: B): (A, B) = ->(y)
  }

  /** @group implicit-classes-any */
  implicit final class Ensuring[A](private val self: A) extends AnyVal {
    def ensuring(cond: Boolean): A = { assert(cond); self }
    def ensuring(cond: Boolean, msg: => Any): A = { assert(cond, msg); self }
    def ensuring(cond: A => Boolean): A = { assert(cond(self)); self }
    def ensuring(cond: A => Boolean, msg: => Any): A = { assert(cond(self), msg); self }
  }

  /** @group implicit-classes-any */
  implicit final class StringFormat[A](private val self: A) extends AnyVal {
    /** Returns string formatted according to given `format` string.
     *  Format strings are as for `String.format`
     *  (@see java.lang.String.format).
     */
    @deprecated("Use `formatString.format(value)` instead of `value.formatted(formatString)`,\nor use the `f\"\"` string interpolator. In Java 15 and later, `formatted` resolves to the new method in String which has reversed parameters.", "2.12.16")
    @inline def formatted(fmtstr: String): String = fmtstr format self
  }

  /** Injects String concatenation operator `+` to any classes.
   * @group implicit-classes-any
   */
  @(deprecated @companionMethod)("Implicit injection of + is deprecated. Convert to String to call +", "2.13.0")
  @(deprecated @companionClass)("Implicit injection of + is deprecated. Convert to String to call +", "2.13.0") // for Scaladoc
  // scala/bug#8229 retaining the pre 2.11 name for source compatibility in shadowing this implicit
  implicit final class any2stringadd[A](private val self: A) extends AnyVal {
    def +(other: String): String = String.valueOf(self) + other
  }

  /** @group char-sequence-wrappers */
  final class SeqCharSequence(sequenceOfChars: scala.collection.IndexedSeq[Char]) extends CharSequence {
    def length: Int                                     = sequenceOfChars.length
    def charAt(index: Int): Char                        = sequenceOfChars(index)
    def subSequence(start: Int, end: Int): CharSequence = new SeqCharSequence(sequenceOfChars.slice(start, end))
    override def toString                               = sequenceOfChars.mkString
  }

  /** @group char-sequence-wrappers */
  def SeqCharSequence(sequenceOfChars: scala.collection.IndexedSeq[Char]): SeqCharSequence = new SeqCharSequence(sequenceOfChars)

  /** @group char-sequence-wrappers */
  final class ArrayCharSequence(arrayOfChars: Array[Char]) extends CharSequence {
    def length: Int                                     = arrayOfChars.length
    def charAt(index: Int): Char                        = arrayOfChars(index)
    def subSequence(start: Int, end: Int): CharSequence = new runtime.ArrayCharSequence(arrayOfChars, start, end)
    override def toString                               = arrayOfChars.mkString
  }

  /** @group char-sequence-wrappers */
  def ArrayCharSequence(arrayOfChars: Array[Char]): ArrayCharSequence = new ArrayCharSequence(arrayOfChars)

  /** @group conversions-string */
  @inline implicit def augmentString(x: String): StringOps = new StringOps(x)

  // printing -----------------------------------------------------------

  /** Prints an object to `out` using its `toString` method.
   *
   *  @param x the object to print; may be null.
   *  @group console-output
   */
  def print(x: Any): Unit = Console.print(x)

  /** Prints a newline character on the default output.
   *  @group console-output
   */
  def println(): Unit = Console.println()

  /** Prints out an object to the default output, followed by a newline character.
   *
   *  @param x the object to print.
   *  @group console-output
   */
  def println(x: Any): Unit = Console.println(x)

  /** Prints its arguments as a formatted string to the default output,
   *  based on a string pattern (in a fashion similar to printf in C).
   *
   *  The interpretation of the formatting patterns is described in
   *  [[java.util.Formatter]].
   *
   *  Consider using the [[scala.StringContext.f f interpolator]] as more type safe and idiomatic.
   *
   *  @param text the pattern for formatting the arguments.
   *  @param xs   the arguments used to instantiate the pattern.
   *  @throws java.lang.IllegalArgumentException if there was a problem with the format string or arguments
   *
   *  @see [[scala.StringContext.f StringContext.f]]
   *  @group console-output
   */
  def printf(text: String, xs: Any*): Unit = Console.print(text.format(xs*))

  // views --------------------------------------------------------------

  // these two are morally deprecated but the @deprecated annotation has been moved to the extension method themselves,
  // in order to provide a more specific deprecation method.
  implicit def tuple2ToZippedOps[T1, T2](x: (T1, T2)): runtime.Tuple2Zipped.Ops[T1, T2]             = new runtime.Tuple2Zipped.Ops(x)
  implicit def tuple3ToZippedOps[T1, T2, T3](x: (T1, T2, T3)): runtime.Tuple3Zipped.Ops[T1, T2, T3] = new runtime.Tuple3Zipped.Ops(x)

  // Not specialized anymore since 2.13 but we still need separate methods
  // to avoid https://github.com/scala/bug/issues/10746
  // TODO: should not need @inline. add heuristic to inline factories for value classes.
  @inline implicit def genericArrayOps[T](xs: Array[T]): ArrayOps[T]          = new ArrayOps(xs)
  @inline implicit def booleanArrayOps(xs: Array[Boolean]): ArrayOps[Boolean] = new ArrayOps(xs)
  @inline implicit def byteArrayOps(xs: Array[Byte]): ArrayOps[Byte]          = new ArrayOps(xs)
  @inline implicit def charArrayOps(xs: Array[Char]): ArrayOps[Char]          = new ArrayOps(xs)
  @inline implicit def doubleArrayOps(xs: Array[Double]): ArrayOps[Double]    = new ArrayOps(xs)
  @inline implicit def floatArrayOps(xs: Array[Float]): ArrayOps[Float]       = new ArrayOps(xs)
  @inline implicit def intArrayOps(xs: Array[Int]): ArrayOps[Int]             = new ArrayOps(xs)
  @inline implicit def longArrayOps(xs: Array[Long]): ArrayOps[Long]          = new ArrayOps(xs)
  @inline implicit def refArrayOps[T <: AnyRef | Null](xs: Array[T]): ArrayOps[T]    = new ArrayOps(xs)
  @inline implicit def shortArrayOps(xs: Array[Short]): ArrayOps[Short]       = new ArrayOps(xs)
  @inline implicit def unitArrayOps(xs: Array[Unit]): ArrayOps[Unit]          = new ArrayOps(xs)

  // "Autoboxing" and "Autounboxing" ---------------------------------------------------

  /** @group conversions-anyval-to-java */
  implicit def byte2Byte(x: Byte): java.lang.Byte             = x.asInstanceOf[java.lang.Byte]
  /** @group conversions-anyval-to-java */
  implicit def short2Short(x: Short): java.lang.Short         = x.asInstanceOf[java.lang.Short]
  /** @group conversions-anyval-to-java */
  implicit def char2Character(x: Char): java.lang.Character   = x.asInstanceOf[java.lang.Character]
  /** @group conversions-anyval-to-java */
  implicit def int2Integer(x: Int): java.lang.Integer         = x.asInstanceOf[java.lang.Integer]
  /** @group conversions-anyval-to-java */
  implicit def long2Long(x: Long): java.lang.Long             = x.asInstanceOf[java.lang.Long]
  /** @group conversions-anyval-to-java */
  implicit def float2Float(x: Float): java.lang.Float         = x.asInstanceOf[java.lang.Float]
  /** @group conversions-anyval-to-java */
  implicit def double2Double(x: Double): java.lang.Double     = x.asInstanceOf[java.lang.Double]
  /** @group conversions-anyval-to-java */
  implicit def boolean2Boolean(x: Boolean): java.lang.Boolean = x.asInstanceOf[java.lang.Boolean]

  /** @group conversions-java-to-anyval */
  implicit def Byte2byte(x: java.lang.Byte): Byte             = x.asInstanceOf[Byte]
  /** @group conversions-java-to-anyval */
  implicit def Short2short(x: java.lang.Short): Short         = x.asInstanceOf[Short]
  /** @group conversions-java-to-anyval */
  implicit def Character2char(x: java.lang.Character): Char   = x.asInstanceOf[Char]
  /** @group conversions-java-to-anyval */
  implicit def Integer2int(x: java.lang.Integer): Int         = x.asInstanceOf[Int]
  /** @group conversions-java-to-anyval */
  implicit def Long2long(x: java.lang.Long): Long             = x.asInstanceOf[Long]
  /** @group conversions-java-to-anyval */
  implicit def Float2float(x: java.lang.Float): Float         = x.asInstanceOf[Float]
  /** @group conversions-java-to-anyval */
  implicit def Double2double(x: java.lang.Double): Double     = x.asInstanceOf[Double]
  /** @group conversions-java-to-anyval */
  implicit def Boolean2boolean(x: java.lang.Boolean): Boolean = x.asInstanceOf[Boolean]

  /** An implicit of type `A => A` is available for all `A` because it can always
   *  be implemented using the identity function. This also means that an
   *  implicit of type `A => B` is always available when `A <: B`, because
   *  `(A => A) <: (A => B)`.
   */
  // $ to avoid accidental shadowing (e.g. scala/bug#7788)
  implicit def $conforms[A]: A => A = <:<.refl

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

}

/** The `LowPriorityImplicits` class provides implicit values that
*  are valid in all Scala compilation units without explicit qualification,
*  but that are partially overridden by higher-priority conversions in object
*  `Predef`.
*/
// scala/bug#7335 Parents of Predef are defined in the same compilation unit to avoid
// cyclic reference errors compiling the standard library *without* a previously
// compiled copy on the classpath.
private[scala] abstract class LowPriorityImplicits extends LowPriorityImplicits2 {
  import mutable.ArraySeq

  // Deprecated conversions to runtime.Rich* classes; these methods used to be `implicit`
  @deprecated("use the extension methods available on primitive types instead", since = "3.8.0")
  @inline def byteWrapper(x: Byte): runtime.RichByte          = new runtime.RichByte(x)
  @deprecated("use the extension methods available on primitive types instead", since = "3.8.0")
  @inline def shortWrapper(x: Short): runtime.RichShort       = new runtime.RichShort(x)
  @deprecated("use the extension methods available on primitive types instead", since = "3.8.0")
  @inline def intWrapper(x: Int): runtime.RichInt             = new runtime.RichInt(x)
  @deprecated("use the extension methods available on primitive types instead", since = "3.8.0")
  @inline def charWrapper(c: Char): runtime.RichChar          = new runtime.RichChar(c)
  @deprecated("use the extension methods available on primitive types instead", since = "3.8.0")
  @inline def longWrapper(x: Long): runtime.RichLong          = new runtime.RichLong(x)
  @deprecated("use the extension methods available on primitive types instead", since = "3.8.0")
  @inline def floatWrapper(x: Float): runtime.RichFloat       = new runtime.RichFloat(x)
  @deprecated("use the extension methods available on primitive types instead", since = "3.8.0")
  @inline def doubleWrapper(x: Double): runtime.RichDouble    = new runtime.RichDouble(x)
  @deprecated("use the extension methods available on primitive types instead", since = "3.8.0")
  @inline def booleanWrapper(x: Boolean): runtime.RichBoolean = new runtime.RichBoolean(x)

  /** @group conversions-array-to-wrapped-array */
  implicit def genericWrapArray[T](xs: Array[T]): ArraySeq[T] =
    mapNull(xs, ArraySeq.make(xs))

  // Since the JVM thinks arrays are covariant, one 0-length Array[AnyRef]
  // is as good as another for all T <: AnyRef.  Instead of creating 100,000,000
  // unique ones by way of this implicit, let's share one.
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapRefArray[T <: AnyRef | Null](xs: Array[T]): ArraySeq.ofRef[T] =
    mapNull(xs,
      if (xs.length == 0) ArraySeq.empty[AnyRef].asInstanceOf[ArraySeq.ofRef[T]]
      else new ArraySeq.ofRef[T](xs))

  /** @group conversions-array-to-wrapped-array */
  implicit def wrapIntArray(xs: Array[Int]): ArraySeq.ofInt = mapNull(xs, new ArraySeq.ofInt(xs))
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapDoubleArray(xs: Array[Double]): ArraySeq.ofDouble = mapNull(xs, new ArraySeq.ofDouble(xs))
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapLongArray(xs: Array[Long]): ArraySeq.ofLong = mapNull(xs, new ArraySeq.ofLong(xs))
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapFloatArray(xs: Array[Float]): ArraySeq.ofFloat = mapNull(xs, new ArraySeq.ofFloat(xs))
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapCharArray(xs: Array[Char]): ArraySeq.ofChar = mapNull(xs, new ArraySeq.ofChar(xs))
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapByteArray(xs: Array[Byte]): ArraySeq.ofByte = mapNull(xs, new ArraySeq.ofByte(xs))
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapShortArray(xs: Array[Short]): ArraySeq.ofShort = mapNull(xs, new ArraySeq.ofShort(xs))
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapBooleanArray(xs: Array[Boolean]): ArraySeq.ofBoolean = mapNull(xs, new ArraySeq.ofBoolean(xs))
  /** @group conversions-array-to-wrapped-array */
  implicit def wrapUnitArray(xs: Array[Unit]): ArraySeq.ofUnit = mapNull(xs, new ArraySeq.ofUnit(xs))

  /** @group conversions-string */
  implicit def wrapString(s: String): WrappedString = mapNull(s, new WrappedString(s))
}

private[scala] abstract class LowPriorityImplicits2 {
  @deprecated("implicit conversions from Array to immutable.IndexedSeq are implemented by copying; use `toIndexedSeq` explicitly if you want to copy, or use the more efficient non-copying ArraySeq.unsafeWrapArray", since="2.13.0")
  implicit def copyArrayToImmutableIndexedSeq[T](xs: Array[T]): IndexedSeq[T] =
    mapNull(xs, new ArrayOps(xs).toIndexedSeq)
}
