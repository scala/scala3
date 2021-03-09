package dotty.tools
package dotc
package core

import scala.annotation.{threadUnsafe => tu}
import Comments.Comment
import util.Spans.NoSpan

@tu lazy val syntheticSymbolsToComment: Definitions => Map[Symbols.Symbol, Comment] = { (defn: Definitions) =>
  import defn._
  Map(
    AnyClass ->
    """/** Class `Any` is the root of the Scala class hierarchy.  Every class in a Scala
      | *  execution environment inherits directly or indirectly from this class.
      | *
      | * Starting with Scala 2.10 it is possible to directly extend `Any` using ''universal traits''.
      | * A ''universal trait'' is a trait that extends `Any`, only has `def`s as members, and does no initialization.
      | *
      | * The main use case for universal traits is to allow basic inheritance of methods for [[scala.AnyVal value classes]].
      | * For example,
      | *
      | * {{{
      | *     trait Printable extends Any {
      | *       def print(): Unit = println(this)
      | *     }
      | *     class Wrapper(val underlying: Int) extends AnyVal with Printable
      | *
      | *     val w = new Wrapper(3)
      | *     w.print()
      | * }}}
      | *
      | * See the [[https://docs.scala-lang.org/overviews/core/value-classes.html Value Classes and Universal Traits]] for more
      | * details on the interplay of universal traits and value classes.
      | */
    """.stripMargin,

    Any_== ->
    """/** Test two objects for equality.
      | *  The expression `x == that` is equivalent to `if (x eq null) that eq null else x.equals(that)`.
      | *
      | *  @param  that  the object to compare against this object for equality.
      | *  @return       `true` if the receiver object is equivalent to the argument; `false` otherwise.
      | */
    """.stripMargin,

    Any_!= ->
    """/** Test two objects for inequality.
      | *
      | *  @param  that  the object to compare against this object for equality.
      | *  @return       `true` if !(this == that), false otherwise.
      | */
    """.stripMargin,

    Any_equals ->
    """/** Compares the receiver object (`this`) with the argument object (`that`) for equivalence.
      | *
      | *  Any implementation of this method should be an [[https://en.wikipedia.org/wiki/Equivalence_relation equivalence relation]]:
      | *
      | *  - It is reflexive: for any instance `x` of type `Any`, `x.equals(x)` should return `true`.
      | *  - It is symmetric: for any instances `x` and `y` of type `Any`, `x.equals(y)` should return `true` if and
      | *    only if `y.equals(x)` returns `true`.
      | *  - It is transitive: for any instances `x`, `y`, and `z` of type `Any` if `x.equals(y)` returns `true` and
      | *    `y.equals(z)` returns `true`, then `x.equals(z)` should return `true`.
      | *
      | *  If you override this method, you should verify that your implementation remains an equivalence relation.
      | *  Additionally, when overriding this method it is usually necessary to override `hashCode` to ensure that
      | *  objects which are "equal" (`o1.equals(o2)` returns `true`) hash to the same [[scala.Int]].
      | *  (`o1.hashCode.equals(o2.hashCode)`).
      | *
      | *  @param  that    the object to compare against this object for equality.
      | *  @return         `true` if the receiver object is equivalent to the argument; `false` otherwise.
      | */
    """.stripMargin,

    Any_hashCode ->
    """/** Calculate a hash code value for the object.
      | *
      | *  The default hashing algorithm is platform dependent.
      | *
      | *  Note that it is allowed for two objects to have identical hash codes (`o1.hashCode.equals(o2.hashCode)`) yet
      | *  not be equal (`o1.equals(o2)` returns `false`).  A degenerate implementation could always return `0`.
      | *  However, it is required that if two objects are equal (`o1.equals(o2)` returns `true`) that they have
      | *  identical hash codes (`o1.hashCode.equals(o2.hashCode)`).  Therefore, when overriding this method, be sure
      | *  to verify that the behavior is consistent with the `equals` method.
      | *
      | *  @return   the hash code value for this object.
      | */
    """.stripMargin,

    Any_toString ->
    """/** Returns a string representation of the object.
      | *
      | *  The default representation is platform dependent.
      | *
      | *  @return a string representation of the object.
      | */
    """.stripMargin,

    Any_## ->
    """/** Equivalent to `x.hashCode` except for boxed numeric types and `null`.
      | *  For numerics, it returns a hash value which is consistent
      | *  with value equality: if two value type instances compare
      | *  as true, then ## will produce the same hash value for each
      | *  of them.
      | *  For `null` returns a hashcode where `null.hashCode` throws a
      | *  `NullPointerException`.
      | *
      | *  @return   a hash value consistent with ==
      | */
    """.stripMargin,

    Any_isInstanceOf ->
    """/** Test whether the dynamic type of the receiver object is `T0`.
      | *
      | *  Note that the result of the test is modulo Scala's erasure semantics.
      | *  Therefore the expression `1.isInstanceOf[String]` will return `false`, while the
      | *  expression `List(1).isInstanceOf[List[String]]` will return `true`.
      | *  In the latter example, because the type argument is erased as part of compilation it is
      | *  not possible to check whether the contents of the list are of the specified type.
      | *
      | *  @return `true` if the receiver object is an instance of erasure of type `T0`; `false` otherwise.
      | */
    """.stripMargin,

    Any_asInstanceOf ->
    """/** Cast the receiver object to be of type `T0`.
      | *
      | *  Note that the success of a cast at runtime is modulo Scala's erasure semantics.
      | *  Therefore the expression `1.asInstanceOf[String]` will throw a `ClassCastException` at
      | *  runtime, while the expression `List(1).asInstanceOf[List[String]]` will not.
      | *  In the latter example, because the type argument is erased as part of compilation it is
      | *  not possible to check whether the contents of the list are of the requested type.
      | *
      | *  @throws ClassCastException if the receiver object is not an instance of the erasure of type `T0`.
      | *  @return the receiver object.
      | */
    """.stripMargin,

    Any_getClass ->
    """/** Returns the runtime class representation of the object.
      | *
      | *  @return a class object corresponding to the runtime type of the receiver.
      | */
    """.stripMargin,

    MatchableClass ->
    """/**
      | *
      | *
      | *
      | *
      | *
      | *
      | *
      | */
    """.stripMargin,

    AnyRefAlias ->
    """/** Class `AnyRef` is the root class of all ''reference types''.
      | *  All types except the value types descend from this class.
      | *  @template
      | */
    """.stripMargin,

    Object_eq ->
    """/** Tests whether the argument (`that`) is a reference to the receiver object (`this`).
      | *
      | *  The `eq` method implements an [[https://en.wikipedia.org/wiki/Equivalence_relation equivalence relation]] on
      | *  non-null instances of `AnyRef`, and has three additional properties:
      | *
      | *   - It is consistent: for any non-null instances `x` and `y` of type `AnyRef`, multiple invocations of
      | *     `x.eq(y)` consistently returns `true` or consistently returns `false`.
      | *   - For any non-null instance `x` of type `AnyRef`, `x.eq(null)` and `null.eq(x)` returns `false`.
      | *   - `null.eq(null)` returns `true`.
      | *
      | *  When overriding the `equals` or `hashCode` methods, it is important to ensure that their behavior is
      | *  consistent with reference equality.  Therefore, if two objects are references to each other (`o1 eq o2`), they
      | *  should be equal to each other (`o1 == o2`) and they should hash to the same value (`o1.hashCode == o2.hashCode`).
      | *
      | *  @param  that    the object to compare against this object for reference equality.
      | *  @return         `true` if the argument is a reference to the receiver object; `false` otherwise.
      | */
    """.stripMargin,

    Object_ne ->
    """/** Equivalent to `!(this eq that)`.
      | *
      | *  @param  that    the object to compare against this object for reference equality.
      | *  @return         `true` if the argument is not a reference to the receiver object; `false` otherwise.
      | */
    """.stripMargin,

    Object_synchronized ->
    """/** Executes the code in `body` with an exclusive lock on `this`.
      | *
      | *  @param    body    the code to execute
      | *  @return           the result of `body`
      | */
    """.stripMargin,

    Object_clone ->
    """/** Create a copy of the receiver object.
      | *
      | *  The default implementation of the `clone` method is platform dependent.
      | *
      | *  @note   not specified by SLS as a member of AnyRef
      | *  @return a copy of the receiver object.
      | */
    """.stripMargin,

    Object_finalize ->
    """/** Called by the garbage collector on the receiver object when there
      | *  are no more references to the object.
      | *
      | *  The details of when and if the `finalize` method is invoked, as
      | *  well as the interaction between `finalize` and non-local returns
      | *  and exceptions, are all platform dependent.
      | *
      | *  @note   not specified by SLS as a member of AnyRef
      | */
    """.stripMargin,

    Object_notify ->
    """/** Wakes up a single thread that is waiting on the receiver object's monitor.
      | *
      | *  @note   not specified by SLS as a member of AnyRef
      | */
    """.stripMargin,

    Object_notifyAll ->
    """/** Wakes up all threads that are waiting on the receiver object's monitor.
      | *
      | *  @note   not specified by SLS as a member of AnyRef
      | */
    """.stripMargin,

    Object_wait ->
    """/** See [[https://docs.oracle.com/javase/8/docs/api/java/lang/Object.html#wait--]].
      | *
      | *  @note   not specified by SLS as a member of AnyRef
      | */
    """.stripMargin,

    Object_waitL ->
    """/** See [[https://docs.oracle.com/javase/8/docs/api/java/lang/Object.html#wait-long-]].
      | *
      | * @param timeout the maximum time to wait in milliseconds.
      | * @note not specified by SLS as a member of AnyRef
      | */
    """.stripMargin,

    Object_waitLI ->
    """/** See [[https://docs.oracle.com/javase/8/docs/api/java/lang/Object.html#wait-long-int-]]
      | *
      | * @param timeout the maximum time to wait in milliseconds.
      | * @param nanos   additional time, in nanoseconds range 0-999999.
      | * @note not specified by SLS as a member of AnyRef
      | */
    """.stripMargin,

    AnyKindClass ->
    """/**
      | *
      | *
      | *
      | *
      | *
      | *
      | *
      | */
    """.stripMargin,

    andType ->
    """/**
      | *
      | *
      | *
      | *
      | *
      | *
      | *
      | */
    """.stripMargin,

    orType ->
    """/**
      | *
      | *
      | *
      | *
      | *
      | *
      | *
      | */
    """.stripMargin,

    AnyValClass ->
    """/** `AnyVal` is the root class of all ''value types'', which describe values
      | *  not implemented as objects in the underlying host system. Value classes
      | *  are specified in Scala Language Specification, section 12.2.
      | *
      | *  The standard implementation includes nine `AnyVal` subtypes:
      | *
      | *  [[scala.Double]], [[scala.Float]], [[scala.Long]], [[scala.Int]], [[scala.Char]],
      | *  [[scala.Short]], and [[scala.Byte]] are the ''numeric value types''.
      | *
      | *  [[scala.Unit]] and [[scala.Boolean]] are the ''non-numeric value types''.
      | *
      | *  Other groupings:
      | *
      | *   - The ''subrange types'' are [[scala.Byte]], [[scala.Short]], and [[scala.Char]].
      | *   - The ''integer types'' include the subrange types as well as [[scala.Int]] and [[scala.Long]].
      | *   - The ''floating point types'' are [[scala.Float]] and [[scala.Double]].
      | *
      | * Prior to Scala 2.10, `AnyVal` was a sealed trait. Beginning with Scala 2.10,
      | * however, it is possible to define a subclass of `AnyVal` called a ''user-defined value class''
      | * which is treated specially by the compiler. Properly-defined user value classes provide a way
      | * to improve performance on user-defined types by avoiding object allocation at runtime, and by
      | * replacing virtual method invocations with static method invocations.
      | *
      | * User-defined value classes which avoid object allocation...
      | *
      | *   - must have a single `val` parameter that is the underlying runtime representation.
      | *   - can define `def`s, but no `val`s, `var`s, or nested `traits`s, `class`es or `object`s.
      | *   - typically extend no other trait apart from `AnyVal`.
      | *   - cannot be used in type tests or pattern matching.
      | *   - may not override `equals` or `hashCode` methods.
      | *
      | * A minimal example:
      | * {{{
      | *     class Wrapper(val underlying: Int) extends AnyVal {
      | *       def foo: Wrapper = new Wrapper(underlying * 19)
      | *     }
      | * }}}
      | *
      | * It's important to note that user-defined value classes are limited, and in some circumstances,
      | * still must allocate a value class instance at runtime. These limitations and circumstances are
      | * explained in greater detail in the [[https://docs.scala-lang.org/overviews/core/value-classes.html Value Classes and Universal Traits]].
      | */
    """.stripMargin,

    NullClass ->
    """/** `Null` is - together with [[scala.Nothing]] - at the bottom of the Scala type hierarchy.
      | *
      | * `Null` is the type of the `null` literal. It is a subtype of every type
      | * except those of value classes. Value classes are subclasses of [[AnyVal]], which includes
      | * primitive types such as [[Int]], [[Boolean]], and user-defined value classes.
      | *
      | * Since `Null` is not a subtype of value types, `null` is not a member of any such type.
      | * For instance, it is not possible to assign `null` to a variable of type [[scala.Int]].
      | */
    """.stripMargin,

    NothingClass ->
    """/** `Nothing` is - together with [[scala.Null]] - at the bottom of Scala's type hierarchy.
      | *
      | *  `Nothing` is a subtype of every other type (including [[scala.Null]]); there exist
      | *  ''no instances'' of this type.  Although type `Nothing` is uninhabited, it is
      | *  nevertheless useful in several ways.  For instance, the Scala library defines a value
      | *  [[scala.collection.immutable.Nil]] of type `List[Nothing]`. Because lists are covariant in Scala,
      | *  this makes [[scala.collection.immutable.Nil]] an instance of `List[T]`, for any element of type `T`.
      | *
      | *  Another usage for Nothing is the return type for methods which never return normally.
      | *  One example is method error in [[scala.sys]], which always throws an exception.
      | */
    """.stripMargin,

    SingletonClass ->
    """/** `Singleton` is used by the compiler as a supertype for singleton types. This includes literal types,
      | * as they are also singleton types.
      | *
      | * {{{
      | * scala> object A { val x = 42 }
      | * defined object A
      | *
      | * scala> implicitly[A.type <:< Singleton]
      | * res12: A.type <:< Singleton = generalized constraint
      | *
      | * scala> implicitly[A.x.type <:< Singleton]
      | * res13: A.x.type <:< Singleton = generalized constraint
      | *
      | * scala> implicitly[42 <:< Singleton]
      | * res14: 42 <:< Singleton = generalized constraint
      | *
      | * scala> implicitly[Int <:< Singleton]
      | * ^
      | * error: Cannot prove that Int <:< Singleton.
      | * }}}
      | *
      | * `Singleton` has a special meaning when it appears as an upper bound on a formal type
      | * parameter. Normally, type inference in Scala widens singleton types to the underlying
      | * non-singleton type. When a type parameter has an explicit upper bound of `Singleton`,
      | * the compiler infers a singleton type.
      | *
      | * {{{
      | * scala> def check42[T](x: T)(implicit ev: T =:= 42): T = x
      | * check42: [T](x: T)(implicit ev: T =:= 42)T
      | *
      | * scala> val x1 = check42(42)
      | * ^
      | * error: Cannot prove that Int =:= 42.
      | *
      | * scala> def singleCheck42[T <: Singleton](x: T)(implicit ev: T =:= 42): T = x
      | * singleCheck42: [T <: Singleton](x: T)(implicit ev: T =:= 42)T
      | *
      | * scala> val x2 = singleCheck42(42)
      | * x2: Int = 42
      | * }}}
      | *
      | * See also [[https://docs.scala-lang.org/sips/42.type.html SIP-23 about Literal-based Singleton Types]].
      | */
    """.stripMargin,
  ).view.mapValues(Comment(NoSpan, _)).toMap
}
