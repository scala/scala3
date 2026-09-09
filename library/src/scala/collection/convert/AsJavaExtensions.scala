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

import java.util.{concurrent => juc}
import java.{lang => jl, util => ju}

/** Defines `asJava` extension methods, available through [[scala.jdk.CollectionConverters]]. */
trait AsJavaExtensions {
  import scala.jdk.javaapi.{CollectionConverters => conv}

  /** Provides `asJava` and `asJavaEnumeration` extension methods that convert a Scala `Iterator`
   *  to a Java `Iterator` or `Enumeration`.
   *
   *  Each conversion returns a wrapper, not a copy: advancing the result consumes the underlying
   *  Scala iterator, and vice versa. An iterator that was itself obtained through the
   *  corresponding `asScala` conversion is unwrapped, returning the original Java object.
   *
   *  @tparam A the element type of the iterator
   *  @param i the Scala `Iterator` to convert
   */
  implicit class IteratorHasAsJava[A](i: Iterator[A]) {
    /** Converts a Scala `Iterator` to a Java `Iterator`, see
      * [[AsJavaConverters.asJava[A](i:Iterator[A])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: ju.Iterator[A] = conv.asJava(i)

    /** Converts a Scala `Iterator` to a Java `Enumeration`, see
      * [[AsJavaConverters.asJavaEnumeration `scala.jdk.javaapi.CollectionConverters.asJavaEnumeration`]].
      */
    def asJavaEnumeration: ju.Enumeration[A] = conv.asJavaEnumeration(i)
  }

  /** Provides `asJava` and `asJavaCollection` extension methods that convert a Scala `Iterable`
   *  to a Java `Iterable` or `Collection`.
   *
   *  Each conversion returns a wrapper, not a copy: the result is backed by the original Scala
   *  collection, so changes to that collection are visible through the Java view. An iterable
   *  that was itself obtained through the corresponding `asScala` conversion is unwrapped,
   *  returning the original Java object.
   *
   *  @tparam A the element type of the collection
   *  @param i the Scala `Iterable` to convert
   */
  implicit class IterableHasAsJava[A](i: Iterable[A]) {
    /** Converts a Scala `Iterable` to a Java `Iterable`, see
      * [[AsJavaConverters.asJava[A](i:Iterable[A])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: jl.Iterable[A] = conv.asJava(i)

    /** Converts a Scala `Iterator` to a Java `Collection`, see
      * [[AsJavaConverters.asJavaCollection `scala.jdk.javaapi.CollectionConverters.asJavaCollection`]].
      */
    def asJavaCollection: ju.Collection[A] = conv.asJavaCollection(i)
  }

  /** Provides the `asJava` extension method that converts a Scala mutable `Buffer` to a Java
   *  `List`.
   *
   *  The conversion returns a wrapper, not a copy: changes made through either interface are
   *  visible through the other. A buffer that was itself obtained through `asScala` is
   *  unwrapped, returning the original Java `List`.
   *
   *  @tparam A the element type of the buffer
   *  @param b the Scala `Buffer` to convert
   */
  implicit class BufferHasAsJava[A](b: mutable.Buffer[A]) {
    /** Converts a Scala `Buffer` to a Java `List`, see
      * [[AsJavaConverters.asJava[A](b:scala\.collection\.mutable\.Buffer[A])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: ju.List[A] = conv.asJava(b)
  }

  /** Provides the `asJava` extension method that converts a Scala mutable `Seq` to a Java
   *  `List`.
   *
   *  The conversion returns a wrapper, not a copy: changes made through either interface are
   *  visible through the other. A sequence that was itself obtained through `asScala` is
   *  unwrapped, returning the original Java `List`.
   *
   *  @tparam A the element type of the sequence
   *  @param s the Scala mutable `Seq` to convert
   */
  implicit class MutableSeqHasAsJava[A](s: mutable.Seq[A]) {
    /** Converts a Scala `Seq` to a Java `List`, see
      * [[AsJavaConverters.asJava[A](s:scala\.collection\.mutable\.Seq[A])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: ju.List[A] = conv.asJava(s)
  }

  /** Provides the `asJava` extension method that converts a Scala `Seq` to a Java `List`.
   *
   *  The conversion returns a wrapper, not a copy: the result is backed by the original Scala
   *  sequence, so changes to that sequence are visible through the Java view. A sequence that
   *  was itself obtained through `asScala` is unwrapped, returning the original Java `List`.
   *
   *  @tparam A the element type of the sequence
   *  @param s the Scala `Seq` to convert
   */
  implicit class SeqHasAsJava[A](s: Seq[A]) {
    /** Converts a Scala `Seq` to a Java `List`, see
      * [[AsJavaConverters.asJava[A](s:scala\.collection\.Seq[A])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: ju.List[A] = conv.asJava(s)
  }

  /** Provides the `asJava` extension method that converts a Scala mutable `Set` to a Java `Set`.
   *
   *  The conversion returns a wrapper, not a copy: changes made through either interface are
   *  visible through the other. A set that was itself obtained through `asScala` is unwrapped,
   *  returning the original Java `Set`.
   *
   *  @tparam A the element type of the set
   *  @param s the Scala mutable `Set` to convert
   */
  implicit class MutableSetHasAsJava[A](s: mutable.Set[A]) {
    /** Converts a Scala `mutable.Set` to a Java `Set`, see
      * [[AsJavaConverters.asJava[A](s:scala\.collection\.mutable\.Set[A])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: ju.Set[A] = conv.asJava(s)
  }

  /** Provides the `asJava` extension method that converts a Scala `Set` to a Java `Set`.
   *
   *  The conversion returns a wrapper, not a copy: the result is backed by the original Scala
   *  set, so changes to that set are visible through the Java view. A set that was itself
   *  obtained through `asScala` is unwrapped, returning the original Java `Set`.
   *
   *  @tparam A the element type of the set
   *  @param s the Scala `Set` to convert
   */
  implicit class SetHasAsJava[A](s: Set[A]) {
    /** Converts a Scala `Set` to a Java `Set`, see
      * [[AsJavaConverters.asJava[A](s:scala\.collection\.Set[A])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: ju.Set[A] = conv.asJava(s)
  }

  /** Provides `asJava` and `asJavaDictionary` extension methods that convert a Scala mutable
   *  `Map` to a Java `Map` or `Dictionary`.
   *
   *  Each conversion returns a wrapper, not a copy: changes made through either interface are
   *  visible through the other. A map that was itself obtained through the corresponding
   *  `asScala` conversion is unwrapped, returning the original Java object.
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   *  @param m the Scala mutable `Map` to convert
   */
  implicit class MutableMapHasAsJava[K, V](m: mutable.Map[K, V]) {
    /** Converts a Scala `mutable.Map` to a Java `Map`, see
      * [[AsJavaConverters.asJava[K,V](m:scala\.collection\.mutable\.Map[K,V])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: ju.Map[K, V] = conv.asJava(m)

    /** Converts a Scala `mutable.Map` to a Java `Map`, see
      * [[AsJavaConverters.asJavaDictionary `scala.jdk.javaapi.CollectionConverters.asJavaDictionary`]].
      */
    def asJavaDictionary: ju.Dictionary[K, V] = conv.asJavaDictionary(m)
  }

  /** Provides the `asJava` extension method that converts a Scala `Map` to a Java `Map`.
   *
   *  The conversion returns a wrapper, not a copy: the result is backed by the original Scala
   *  map, so changes to that map are visible through the Java view. A map that was itself
   *  obtained through `asScala` is unwrapped, returning the original Java `Map`.
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   *  @param m the Scala `Map` to convert
   */
  implicit class MapHasAsJava[K, V](m: Map[K, V]) {
    /** Converts a Scala `Map` to a Java `Map`, see
      * [[AsJavaConverters.asJava[K,V](m:scala\.collection\.Map[K,V])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: ju.Map[K, V] = conv.asJava(m)
  }

  /** Provides the `asJava` extension method that converts a Scala `concurrent.Map` to a Java
   *  `ConcurrentMap`.
   *
   *  The conversion returns a wrapper, not a copy: changes made through either interface are
   *  visible through the other. A map that was itself obtained through `asScala` is unwrapped,
   *  returning the original Java `ConcurrentMap`.
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   *  @param m the Scala `concurrent.Map` to convert
   */
  implicit class ConcurrentMapHasAsJava[K, V](m: concurrent.Map[K, V]) {
    /** Converts a Scala `concurrent.Map` to a Java `ConcurrentMap`, see
      * [[AsJavaConverters.asJava[K,V](m:scala\.collection\.concurrent\.Map[K,V])* `scala.jdk.javaapi.CollectionConverters.asJava`]].
      */
    def asJava: juc.ConcurrentMap[K, V] = conv.asJava(m)
  }
}
