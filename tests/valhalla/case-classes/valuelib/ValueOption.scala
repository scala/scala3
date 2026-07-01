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

package valuelib

import scala.language.`2.13`
import scala.annotation.valhalla

object ValueOption {

  import scala.language.implicitConversions

  /** An implicit conversion that converts an option to an iterable value. */
  implicit def option2Iterable[A](xo: ValueOption[A]): Iterable[A] =
    if (xo.isEmpty) Iterable.empty else Iterable.single(xo.get)

  /** An ValueOption factory which creates ValueSome(x) if the argument is not null,
   *  and ValueNone if it is null.
   *
   *  @param  x the value
   *  @return   ValueSome(value) if value != null, ValueNone if value == null
   */
  def apply[A](x: A | Null): ValueOption[A] = if (x == null) ValueNone else ValueSome(x)

  /** An ValueOption factory which returns `ValueNone` in a manner consistent with
   *  the collections hierarchy.
   */
  def empty[A] : ValueOption[A] = ValueNone

  /** When a given condition is true, evaluates the `a` argument and returns
   *  `ValueSome(a)`. When the condition is false, `a` is not evaluated and `ValueNone` is
   *  returned.
   */
  def when[A](cond: Boolean)(a: => A): ValueOption[A] =
    if (cond) ValueSome(a) else ValueNone

  /** Unless a given condition is true, this will evaluate the `a` argument and
   *  return `ValueSome(a)`. Otherwise, `a` is not evaluated and `ValueNone` is returned.
   */
  @inline def unless[A](cond: Boolean)(a: => A): ValueOption[A] =
    when(!cond)(a)
}

/** Represents optional values. Instances of `ValueOption`
 *  are either an instance of $some or the object $none.
 *
 *  The most idiomatic way to use an $option instance is to treat it
 *  as a collection or monad and use `map`,`flatMap`, `filter`, or
 *  `foreach`:
 *
 *  ```
 *  val name: ValueOption[String] = request.getParameter("name")
 *  val upper = name.map(_.trim).filter(_.length != 0).map(_.toUpperCase)
 *  println(upper.getOrElse(""))
 *  ```
 *
 *  Note that this is equivalent to ```
 *  val upper = for {
 *    name <- request.getParameter("name")
 *    trimmed <- ValueSome(name.trim)
 *    upper <- ValueSome(trimmed.toUpperCase) if trimmed.length != 0
 *  } yield upper
 *  println(upper.getOrElse(""))
 *  ```
 *
 *  Because of how for comprehension works, if $none is returned
 *  from `request.getParameter`, the entire expression results in
 *  $none
 *
 *  This allows for sophisticated chaining of $option values without
 *  having to check for the existence of a value.
 *
 *  These are useful methods that exist for both $some and $none.
 *  - [[isDefined]] — True if not empty
 *  - [[isEmpty]] — True if empty
 *  - [[nonEmpty]] — True if not empty
 *  - [[orElse]] — Evaluate and return alternate optional value if empty
 *  - [[getOrElse]] — Evaluate and return alternate value if empty
 *  - [[get]] — Return value, throw exception if empty
 *  - [[fold]] —  Apply function on optional value, return default if empty
 *  - [[map]] — Apply a function on the optional value
 *  - [[flatMap]] — Same as map but function must return an optional value
 *  - [[foreach]] — Apply a procedure on option value
 *  - [[collect]] — Apply partial pattern match on optional value
 *  - [[filter]] — An optional value satisfies predicate
 *  - [[filterNot]] — An optional value doesn't satisfy predicate
 *  - [[exists]] — Apply predicate on optional value, or false if empty
 *  - [[forall]] — Apply predicate on optional value, or true if empty
 *  - [[contains]] — Checks if value equals optional value, or false if empty
 *  - [[zip]] — Combine two optional values to make a paired optional value
 *  - [[unzip]] — Split an optional pair to two optional values
 *  - [[unzip3]] — Split an optional triple to three optional values
 *  - [[toList]] — Unary list of optional value, otherwise the empty list
 *
 *  A less-idiomatic way to use $option values is via pattern matching: ```
 *  val nameMaybe = request.getParameter("name")
 *  nameMaybe match {
 *    case ValueSome(name) =>
 *      println(name.trim.toUppercase)
 *    case ValueNone =>
 *      println("No name value")
 *  }
 *  ```
 *
 *  Interacting with code that can occasionally return null can be
 *  safely wrapped in $option to become $none and $some otherwise. ```
 *  val abc = new java.util.HashMap[Int, String]
 *  abc.put(1, "A")
 *  bMaybe = ValueOption(abc.get(2))
 *  bMaybe match {
 *   case ValueSome(b) =>
 *     println(s"Found \$b")
 *   case ValueNone =>
 *     println("Not found")
 *  }
 *  ```
 *
 *  @note Many of the methods in here are duplicative with those
 *  in the Iterable hierarchy, but they are duplicated for a reason:
 *  the implicit conversion tends to leave one with an Iterable in
 *  situations where one could have retained an ValueOption.
 *
 *  @define none `ValueNone`
 *  @define some [[scala.ValueSome]]
 *  @define option [[scala.ValueOption]]
 *  @define p `p`
 *  @define f `f`
 *  @define coll option
 *  @define Coll `ValueOption`
 *  @define orderDependent
 *  @define orderDependentFold
 *  @define mayNotTerminateInf
 *  @define willNotTerminateInf
 *  @define collectExample
 *  @define undefinedorder
 */
@valhalla
@SerialVersionUID(-114498752079829388L) // value computed by serialver for 2.11.2, annotation added in 2.11.4
sealed abstract class ValueOption[+A] extends AnyVal with IterableOnce[A] with Product with Serializable with DeepValhalla {
  self =>

  /** Returns true if the option is $none, false otherwise.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(_) => false
   *   case ValueNone    => true
   *  }
   *  ```
   */
  final def isEmpty: Boolean = this eq ValueNone

  /** Returns true if the option is an instance of $some, false otherwise.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(_) => true
   *   case ValueNone    => false
   *  }
   *  ```
   */
  final def isDefined: Boolean = !isEmpty

  override final def knownSize: Int = if (isEmpty) 0 else 1

  /** Returns the option's value.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(x) => x
   *   case ValueNone    => throw new Exception
   *  }
   *  ```
   *  @note The option must be nonempty.
   *  @throws NoSuchElementException if the option is empty.
   */
  def get: A

  /** Returns the option's value if the option is nonempty, otherwise
   *  return the result of evaluating `default`.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(x) => x
   *   case ValueNone    => default
   *  }
   *  ```
   *
   *  @param default  the default expression.
   */
  @inline final def getOrElse[B >: A](default: => B): B =
    if (isEmpty) default else this.get

  /** Returns the option's value if it is nonempty,
   *  or `null` if it is empty.
   *
   *  Although the use of null is discouraged, code written to use
   *  $option must often interface with code that expects and returns nulls.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(x) => x
   *   case ValueNone    => null
   *  }
   *  ```
   *  @example ```
   *  val initialText: ValueOption[String] = getInitialText
   *  val textField = new JComponent(initialText.orNull,20)
   *  ```
   */
  @inline final def orNull[A1 >: A](implicit ev: Null <:< A1): A1 = this getOrElse ev(null)

  /** Returns a $some containing the result of applying $f to this $option's
   *  value if this $option is nonempty.
   *  Otherwise return $none.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(x) => ValueSome(f(x))
   *   case ValueNone    => ValueNone
   *  }
   *  ```
   *  @note This is similar to `flatMap` except here,
   *  $f does not need to wrap its result in an $option.
   *
   *  @param  f   the function to apply
   *  @see flatMap
   *  @see foreach
   */
  @inline final def map[B](f: A => B): ValueOption[B] =
    if (isEmpty) ValueNone else ValueSome(f(this.get))

  /** Returns the result of applying $f to this $option's
   *  value if the $option is nonempty.  Otherwise, evaluates
   *  expression `ifEmpty`.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(x) => f(x)
   *   case ValueNone    => ifEmpty
   *  }
   *  ```
   *  This is also equivalent to:
   *  ```
   *  option.map(f).getOrElse(ifEmpty)
   *  ```
   *  @param  ifEmpty the expression to evaluate if empty.
   *  @param  f       the function to apply if nonempty.
   */
  @inline final def fold[B](ifEmpty: => B)(f: A => B): B =
    if (isEmpty) ifEmpty else f(this.get)

  /** Returns the result of applying $f to this $option's value if
   *  this $option is nonempty.
   *  Returns $none if this $option is empty.
   *  Slightly different from `map` in that $f is expected to
   *  return an $option (which could be $none).
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(x) => f(x)
   *   case ValueNone    => ValueNone
   *  }
   *  ```
   *  @param  f   the function to apply
   *  @see map
   *  @see foreach
   */
  @inline final def flatMap[B](f: A => ValueOption[B]): ValueOption[B] =
    if (isEmpty) ValueNone else f(this.get)

  /** Returns the nested $option value if it is nonempty.  Otherwise,
   *  return $none.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(ValueSome(b)) => ValueSome(b)
   *   case _             => ValueNone
   *  }
   *  ```
   *  @example ```
   *  ValueSome(ValueSome("something")).flatten
   *  ```
   *
   *  @param ev an implicit conversion that asserts that the value is
   *           also an $option.
   *  @see flatMap
   */
  def flatten[B](implicit ev: A <:< ValueOption[B]): ValueOption[B] =
    if (isEmpty) ValueNone else ev(this.get)

  /** Returns this $option if it is nonempty **and** applying the predicate $p to
   *  this $option's value returns true. Otherwise, return $none.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(x) if p(x) => ValueSome(x)
   *   case _               => ValueNone
   *  }
   *  ```
   *  @param  p   the predicate used for testing.
   */
  @inline final def filter(p: A => Boolean): ValueOption[A] =
    if (isEmpty || p(this.get)) this else ValueNone

  /** Returns this $option if it is nonempty **and** applying the predicate $p to
   *  this $option's value returns false. Otherwise, return $none.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(x) if !p(x) => ValueSome(x)
   *   case _                => ValueNone
   *  }
   *  ```
   *  @param  p   the predicate used for testing.
   */
  @inline final def filterNot(p: A => Boolean): ValueOption[A] =
    if (isEmpty || !p(this.get)) this else ValueNone

  /** Returns false if the option is $none, true otherwise.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(_) => true
   *   case ValueNone    => false
   *  }
   *  ```
   *  @note   Implemented here to avoid the implicit conversion to Iterable.
   */
  final def nonEmpty: Boolean = isDefined

  /** Necessary to keep $option from being implicitly converted to
   *  [[scala.collection.Iterable]] in `for` comprehensions.
   */
  @inline final def withFilter(p: A => Boolean): WithFilter = new WithFilter(p)

  /** We need a whole WithFilter class to honor the "doesn't create a new
   *  collection" contract even though it seems unlikely to matter much in a
   *  collection with max size 1.
   */
  class WithFilter(p: A => Boolean) {
    def map[B](f: A => B): ValueOption[B] = self filter p map f
    def flatMap[B](f: A => ValueOption[B]): ValueOption[B] = self filter p flatMap f
    def foreach[U](f: A => U): Unit = self filter p foreach f
    def withFilter(q: A => Boolean): WithFilter = new WithFilter(x => p(x) && q(x))
  }

  /** Tests whether the option contains a given value as an element.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(x) => x == elem
   *   case ValueNone    => false
   *  }
   *  ```
   *  @example ```
   *  // Returns true because ValueSome instance contains string "something" which equals "something".
   *  ValueSome("something") contains "something"
   *
   *  // Returns false because "something" != "anything".
   *  ValueSome("something") contains "anything"
   *
   *  // Returns false when method called on ValueNone.
   *  ValueNone contains "anything"
   *  ```
   *
   *  @param elem the element to test.
   *  @return `true` if the option has an element that is equal (as
   *  determined by `==`) to `elem`, `false` otherwise.
   */
  final def contains[A1 >: A](elem: A1): Boolean =
    !isEmpty && this.get == elem

  /** Returns true if this option is nonempty **and** the predicate
   *  $p returns true when applied to this $option's value.
   *  Otherwise, returns false.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(x) => p(x)
   *   case ValueNone    => false
   *  }
   *  ```
   *  @param  p   the predicate to test
   */
  @inline final def exists(p: A => Boolean): Boolean =
    !isEmpty && p(this.get)

  /** Returns true if this option is empty **or** the predicate
   *  $p returns true when applied to this $option's value.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(x) => p(x)
   *   case ValueNone    => true
   *  }
   *  ```
   *  @param  p   the predicate to test
   */
  @inline final def forall(p: A => Boolean): Boolean = isEmpty || p(this.get)

  /** Applies the given procedure $f to the option's value,
   *  if it is nonempty. Otherwise, do nothing.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(x) => f(x)
   *   case ValueNone    => ()
   *  }
   *  ```
   *  @param  f   the procedure to apply.
   *  @see map
   *  @see flatMap
   */
  @inline final def foreach[U](f: A => U): Unit = {
    if (!isEmpty) f(this.get)
  }

  /** Returns a $some containing the result of
   *  applying `pf` to this $option's contained
   *  value, **if** this option is
   *  nonempty **and** `pf` is defined for that value.
   *  Returns $none otherwise.
   *
   *  @example ```
   *  // Returns ValueSome(HTTP) because the partial function covers the case.
   *  ValueSome("http") collect {case "http" => "HTTP"}
   *
   *  // Returns ValueNone because the partial function doesn't cover the case.
   *  ValueSome("ftp") collect {case "http" => "HTTP"}
   *
   *  // Returns ValueNone because the option is empty. There is no value to pass to the partial function.
   *  ValueNone collect {case value => value}
   *  ```
   *
   *  @param  pf   the partial function.
   *  @return the result of applying `pf` to this $option's
   *  value (if possible), or $none.
   */
  // @inline final def collect[B](pf: ValuePartialFunction[A, B]): ValueOption[B] =
  //   if (!isEmpty) pf.lift(this.get) else ValueNone

  @inline final def collect[B](pf: PartialFunction[A, B]): ValueOption[B] =
    if (!isEmpty) pf.lift(this.get) match {case Some(x) => ValueSome(x)} else ValueNone

  /** Returns this $option if it is nonempty,
   *  otherwise return the result of evaluating `alternative`.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(x) => ValueSome(x)
   *   case ValueNone    => alternative
   *  }
   *  ```
   *  @param alternative the alternative expression.
   */
  @inline final def orElse[B >: A](alternative: => ValueOption[B]): ValueOption[B] =
    if (isEmpty) alternative else this

  /** Returns a $some formed from this option and another option
   *  by combining the corresponding elements in a pair.
   *  If either of the two options is empty, $none is returned.
   *
   *  This is equivalent to:
   *  ```
   *  (option1, option2) match {
   *    case (ValueSome(x), ValueSome(y)) => ValueSome((x, y))
   *    case _                  => ValueNone
   *  }
   *  ```
   *  @example ```
   *  // Returns ValueSome(("foo", "bar")) because both options are nonempty.
   *  ValueSome("foo") zip ValueSome("bar")
   *
   *  // Returns ValueNone because `that` option is empty.
   *  ValueSome("foo") zip ValueNone
   *
   *  // Returns ValueNone because `this` option is empty.
   *  ValueNone zip ValueSome("bar")
   *  ```
   *
   *  @param  that   the options which is going to be zipped
   */
  final def zip[A1 >: A, B](that: ValueOption[B]): ValueOption[(A1, B)] =
    if (isEmpty || that.isEmpty) ValueNone else ValueSome((this.get, that.get))

  /** Converts an ValueOption of a pair into an ValueOption of the first element and an ValueOption of the second element.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *    case ValueSome((x, y)) => (ValueSome(x), ValueSome(y))
   *    case _            => (ValueNone,    ValueNone)
   *  }
   *  ```
   *  @tparam A1    the type of the first half of the element pair
   *  @tparam A2    the type of the second half of the element pair
   *  @param asPair an implicit conversion which asserts that the element type
   *                of this ValueOption is a pair.
   *  @return       a pair of ValueOptions, containing, respectively, the first and second half
   *                of the element pair of this ValueOption.
   */
  final def unzip[A1, A2](implicit asPair: A <:< (A1, A2)): (ValueOption[A1], ValueOption[A2]) = {
    if (isEmpty)
      (ValueNone, ValueNone)
    else {
      val e = asPair(this.get)
      (ValueSome(e._1), ValueSome(e._2))
    }
  }

  /** Converts an ValueOption of a triple into three ValueOptions, one containing the element from each position of the triple.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *    case ValueSome((x, y, z)) => (ValueSome(x), ValueSome(y), ValueSome(z))
   *    case _               => (ValueNone,    ValueNone,    ValueNone)
   *  }
   *  ```
   *  @tparam A1      the type of the first of three elements in the triple
   *  @tparam A2      the type of the second of three elements in the triple
   *  @tparam A3      the type of the third of three elements in the triple
   *  @param asTriple an implicit conversion which asserts that the element type
   *                  of this ValueOption is a triple.
   *  @return         a triple of ValueOptions, containing, respectively, the first, second, and third
   *                  elements from the element triple of this ValueOption.
   */
  final def unzip3[A1, A2, A3](implicit asTriple: A <:< (A1, A2, A3)): (ValueOption[A1], ValueOption[A2], ValueOption[A3]) = {
    if (isEmpty)
      (ValueNone, ValueNone, ValueNone)
    else {
      val e = asTriple(this.get)
      (ValueSome(e._1), ValueSome(e._2), ValueSome(e._3))
    }
  }

  /** Returns a singleton iterator returning the $option's value
   *  if it is nonempty, or an empty iterator if the option is empty.
   */
  def iterator: Iterator[A] =
    if (isEmpty) collection.Iterator.empty else collection.Iterator.single(this.get)

  /** Returns a singleton list containing the $option's value
   *  if it is nonempty, or the empty list if the $option is empty.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(x) => List(x)
   *   case ValueNone    => Nil
   *  }
   *  ```
   */
  def toList: List[A] =
    if (isEmpty) List() else new ::(this.get, Nil)

  /** Returns a [[scala.util.Left]] containing the given
   *  argument `left` if this $option is empty, or
   *  a [[scala.util.Right]] containing this $option's value if
   *  this is nonempty.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(x) => Right(x)
   *   case ValueNone    => Left(left)
   *  }
   *  ```
   *  @param left the expression to evaluate and return if this is empty
   *  @see toLeft
   */
  @inline final def toRight[X](left: => X): Either[X, A] =
    if (isEmpty) Left(left) else Right(this.get)

  /** Returns a [[scala.util.Right]] containing the given
   *  argument `right` if this is empty, or
   *  a [[scala.util.Left]] containing this $option's value
   *  if this $option is nonempty.
   *
   *  This is equivalent to:
   *  ```
   *  option match {
   *   case ValueSome(x) => Left(x)
   *   case ValueNone    => Right(right)
   *  }
   *  ```
   *  @param right the expression to evaluate and return if this is empty
   *  @see toRight
   */
  @inline final def toLeft[X](right: => X): Either[A, X] =
    if (isEmpty) Right(right) else Left(this.get)
}

/** Class `ValueSome[A]` represents existing values of type
 *  `A`.
 */
@valhalla
@SerialVersionUID(1234815782226070388L) // value computed by serialver for 2.11.2, annotation added in 2.11.4
final case class ValueSome[+A](value: A) extends ValueOption[A] {
  def get: A = value
}

@valhalla
/** This case object represents non-existent values. */
@SerialVersionUID(5066590221178148012L) // value computed by serialver for 2.11.2, annotation added in 2.11.4
case object ValueNone extends ValueOption[Nothing] {
  def get: Nothing = throw new NoSuchElementException("ValueNone.get")
}
