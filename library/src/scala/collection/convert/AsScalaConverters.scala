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
package collection
package convert

import scala.language.`2.13`
import language.experimental.captureChecking
import caps.unsafe.unsafeAssumePure

import java.util.{concurrent => juc}
import java.{lang => jl, util => ju}

import scala.{unchecked => uc}

/** Defines converter methods from Java to Scala collections.
 *  These methods are available through the [[scala.jdk.javaapi.CollectionConverters]] object.
 */
trait AsScalaConverters {
  import JavaCollectionWrappers._

  // Note: Both the parameter and the return type of the methods in this class are non-nullable.
  // However, if a null reference is passed explicitly, the method will still return null.
  // We intentionally keep this signature to discourage passing nulls implicitly while preserving the
  // previous behavior for backward compatibility.

  /** Converts a Java `Iterator` to a Scala `Iterator`.
   *
   *  The returned Scala `Iterator` is backed by the provided Java `Iterator` and any side-effects of
   *  using it via the Scala interface will be visible via the Java interface and vice versa.
   *
   *  If the Java `Iterator` was previously obtained from an implicit or explicit call of
   *  `asJava` then the original Scala `Iterator` will be returned.
   *
   *  @tparam A the element type of the iterator
   *  @param i The Java `Iterator` to be converted.
   *  @return  A Scala `Iterator` view of the argument.
   */
  def asScala[A](i: ju.Iterator[A]): Iterator[A] = (i: ju.Iterator[A] | Null) match {
    case null                            => null.asInstanceOf[Iterator[A]]
    case wrapper: IteratorWrapper[A @uc] => wrapper.underlying.unsafeAssumePure // TODO: Remove when pattern matching recognizes this is safe
    case _                               => new JIteratorWrapper(i)
  }

  /** Converts a Java `Enumeration` to a Scala `Iterator`.
   *
   *  The returned Scala `Iterator` is backed by the provided Java `Enumeration` and any side-effects
   *  of using it via the Scala interface will be visible via the Java interface and vice versa.
   *
   *  If the Java `Enumeration` was previously obtained from an implicit or explicit call of
   *  `asJavaEnumeration` then the original Scala `Iterator` will be returned.
   *
   *  @tparam A the element type of the enumeration
   *  @param e The Java `Enumeration` to be converted.
   *  @return  A Scala `Iterator` view of the argument.
   */
  def asScala[A](e: ju.Enumeration[A]): Iterator[A] = (e: ju.Enumeration[A] | Null) match {
    case null                            => null.asInstanceOf[Iterator[A]]
    case wrapper: IteratorWrapper[A @uc] => wrapper.underlying.unsafeAssumePure // TODO: Remove when pattern matching recognizes this is safe
    case _                               => new JEnumerationWrapper(e)
  }

  /** Converts a Java `Iterable` to a Scala `Iterable`.
   *
   *  The returned Scala `Iterable` is backed by the provided Java `Iterable` and any side-effects of
   *  using it via the Scala interface will be visible via the Java interface and vice versa.
   *
   *  If the Java `Iterable` was previously obtained from an implicit or explicit call of
   *  `asJava` then the original Scala `Iterable` will be returned.
   *
   *  @tparam A the element type of the iterable
   *  @param i The Java `Iterable` to be converted.
   *  @return  A Scala `Iterable` view of the argument.
   */
  def asScala[A](i: jl.Iterable[A]): Iterable[A] = (i: jl.Iterable[A] | Null) match {
    case null                            => null.asInstanceOf[Iterable[A]]
    case wrapper: IterableWrapper[A @uc] => wrapper.underlying.unsafeAssumePure // TODO: Remove when pattern matching recognizes this is safe
    case _                               => new JIterableWrapper(i)
  }

  /** Converts a Java `Collection` to a Scala `Iterable`.
   *
   *  If the Java `Collection` was previously obtained from an implicit or explicit call of
   *  `asJavaCollection` then the original Scala `Iterable` will be returned.
   *
   *  @tparam A the element type of the collection
   *  @param c The Java `Collection` to be converted.
   *  @return  A Scala `Iterable` view of the argument.
   */
  def asScala[A](c: ju.Collection[A]): Iterable[A] = (c: ju.Collection[A] | Null) match {
    case null                            => null.asInstanceOf[Iterable[A]]
    case wrapper: IterableWrapper[A @uc] => wrapper.underlying.unsafeAssumePure // TODO: Remove when pattern matching recognizes this is safe
    case _                               => new JCollectionWrapper(c)
  }

  /** Converts a Java `List` to a Scala mutable `Buffer`.
   *
   *  The returned Scala `Buffer` is backed by the provided Java `List` and any side-effects of using
   *  it via the Scala interface will be visible via the Java interface and vice versa.
   *
   *  If the Java `List` was previously obtained from an implicit or explicit call of
   *  `asJava` then the original Scala `Buffer` will be returned.
   *
   *  @tparam A the element type of the list
   *  @param l The Java `List` to be converted.
   *  @return A Scala mutable `Buffer` view of the argument.
   */
  def asScala[A](l: ju.List[A]): mutable.Buffer[A] = (l: ju.List[A] | Null) match {
    case null                                 => null.asInstanceOf[mutable.Buffer[A]]
    case wrapper: MutableBufferWrapper[A @uc] => wrapper.underlying
    case _                                    => new JListWrapper(l)
  }

  /** Converts a Java `Set` to a Scala mutable `Set`.
   *
   *  The returned Scala `Set` is backed by the provided Java `Set` and any side-effects of using it
   *  via the Scala interface will be visible via the Java interface and vice versa.
   *
   *  If the Java `Set` was previously obtained from an implicit or explicit call of
   *  `asJava` then the original Scala `Set` will be returned.
   *
   *  @tparam A the element type of the set
   *  @param s The Java `Set` to be converted.
   *  @return  A Scala mutable `Set` view of the argument.
   */
  def asScala[A](s: ju.Set[A]): mutable.Set[A] = (s: ju.Set[A] | Null) match {
    case null                              => null.asInstanceOf[mutable.Set[A]]
    case wrapper: MutableSetWrapper[A @uc] => wrapper.underlying
    case _                                 => new JSetWrapper(s)
  }

  /** Converts a Java `Map` to a Scala mutable `Map`.
   *
   *  The returned Scala `Map` is backed by the provided Java `Map` and any side-effects of using it
   *  via the Scala interface will be visible via the Java interface and vice versa.
   *
   *  If the Java `Map` was previously obtained from an implicit or explicit call of
   *  `asJava` then the original Scala `Map` will be returned.
   *
   *  If the wrapped map is synchronized (e.g. from `java.util.Collections.synchronizedMap`), it is
   *  your responsibility to wrap all non-atomic operations with `underlying.synchronized`.
   *  This includes `get`, as `java.util.Map`'s API does not allow for an atomic `get` when `null`
   *  values may be present.
   *
   *  @tparam K the type of keys in the map
   *  @tparam V the type of values in the map
   *  @param m The Java `Map` to be converted.
   *  @return  A Scala mutable `Map` view of the argument.
   */
  def asScala[K, V](m: ju.Map[K, V]): mutable.Map[K, V] = (m: ju.Map[K, V] | Null) match {
    case null                                     => null.asInstanceOf[mutable.Map[K, V]]
    case wrapper: MutableMapWrapper[K @uc, V @uc] => wrapper.underlying
    case _                                        => new JMapWrapper(m)
  }

  /** Converts a Java `ConcurrentMap` to a Scala mutable `ConcurrentMap`.
   *
   *  The returned Scala `ConcurrentMap` is backed by the provided Java `ConcurrentMap` and any
   *  side-effects of using it via the Scala interface will be visible via the Java interface and
   *  vice versa.
   *
   *  If the Java `ConcurrentMap` was previously obtained from an implicit or explicit call of
   *  `asJava` then the original Scala `ConcurrentMap` will be returned.
   *
   *  @tparam K the type of keys in the map
   *  @tparam V the type of values in the map
   *  @param m The Java `ConcurrentMap` to be converted.
   *  @return  A Scala mutable `ConcurrentMap` view of the argument.
   */
  def asScala[K, V](m: juc.ConcurrentMap[K, V]): concurrent.Map[K, V] = (m: juc.ConcurrentMap[K, V] | Null) match {
    case null                                        => null.asInstanceOf[concurrent.Map[K, V]]
    case wrapper: ConcurrentMapWrapper[K @uc, V @uc] => wrapper.underlyingConcurrentMap
    case _                                           => new JConcurrentMapWrapper(m)
  }

  /** Converts a Java `Dictionary` to a Scala mutable `Map`.
   *
   *  The returned Scala `Map` is backed by the provided Java `Dictionary` and any side-effects of
   *  using it via the Scala interface will be visible via the Java interface and vice versa.
   *
   *  If the Java `Dictionary` was previously obtained from an implicit or explicit call of
   *  `asJavaDictionary` then the original Scala `Map` will be returned.
   *
   *  @tparam K the type of keys in the dictionary
   *  @tparam V the type of values in the dictionary
   *  @param d The Java `Dictionary` to be converted.
   *  @return  A Scala mutable `Map` view of the argument.
   */
  def asScala[K, V](d: ju.Dictionary[K, V]): mutable.Map[K, V] = (d: ju.Dictionary[K, V] | Null) match {
    case null                                     => null.asInstanceOf[mutable.Map[K, V]]
    case wrapper: DictionaryWrapper[K @uc, V @uc] => wrapper.underlying
    case _                                        => new JDictionaryWrapper(d)
  }

  /** Converts a Java `Properties` to a Scala mutable `Map[String, String]`.
   *
   *  The returned Scala `Map[String, String]` is backed by the provided Java `Properties` and any
   *  side-effects of using it via the Scala interface will be visible via the Java interface and
   *  vice versa.
   *
   *  @param p The Java `Properties` to be converted.
   *  @return  A Scala mutable `Map[String, String]` view of the argument.
   */
  def asScala(p: ju.Properties): mutable.Map[String, String] = (p: ju.Properties | Null) match {
    case null => null.asInstanceOf[mutable.Map[String, String]]
    case _    => new JPropertiesWrapper(p)
  }
}
