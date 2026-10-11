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

/** Defines `asScala` extension methods, available through [[scala.jdk.CollectionConverters]]. */
trait AsScalaExtensions {
  import scala.jdk.javaapi.{CollectionConverters => conv}

  /** Provides the `asScala` extension method that converts a Java `Iterator` to a Scala
   *  `Iterator`.
   *
   *  The conversion returns a wrapper, not a copy: advancing the result consumes the underlying
   *  Java iterator, and vice versa. An iterator that was itself obtained through `asJava` is
   *  unwrapped, returning the original Scala `Iterator`.
   *
   *  @tparam A the element type of the iterator
   *  @param i the Java `Iterator` to convert
   */
  implicit class IteratorHasAsScala[A](i: ju.Iterator[A]) {
    /** Converts a Java `Iterator` to a Scala `Iterator`, see
      * [[AsScalaConverters.asScala[A](i:java\.util\.Iterator[A])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: Iterator[A] = conv.asScala(i)
  }

  /** Provides the `asScala` extension method that converts a Java `Enumeration` to a Scala
   *  `Iterator`.
   *
   *  The conversion returns a wrapper, not a copy: advancing the result consumes the underlying
   *  Java enumeration, and vice versa. An enumeration that was itself obtained through
   *  `asJavaEnumeration` is unwrapped, returning the original Scala `Iterator`.
   *
   *  @tparam A the element type of the enumeration
   *  @param e the Java `Enumeration` to convert
   */
  implicit class EnumerationHasAsScala[A](e: ju.Enumeration[A]) {
    /** Converts a Java `Enumeration` to a Scala `Iterator`, see
      * [[AsScalaConverters.asScala[A](e:java\.util\.Enumeration[A])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: Iterator[A] = conv.asScala(e)
  }

  /** Provides the `asScala` extension method that converts a Java `Iterable` to a Scala
   *  `Iterable`.
   *
   *  The conversion returns a wrapper, not a copy: the result is backed by the original Java
   *  iterable, so changes to it are visible through the Scala view. An iterable that was itself
   *  obtained through `asJava` is unwrapped, returning the original Scala `Iterable`.
   *
   *  @tparam A the element type of the iterable
   *  @param i the Java `Iterable` to convert
   */
  implicit class IterableHasAsScala[A](i: jl.Iterable[A]) {
    /** Converts a Java `Iterable` to a Scala `Iterable`, see
      * [[AsScalaConverters.asScala[A](i:Iterable[A])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: Iterable[A] = conv.asScala(i)
  }

  /** Provides the `asScala` extension method that converts a Java `Collection` to a Scala
   *  `Iterable`.
   *
   *  The conversion returns a wrapper, not a copy: the result is backed by the original Java
   *  collection, so changes to it are visible through the Scala view. A collection that was
   *  itself obtained through `asJavaCollection` is unwrapped, returning the original Scala
   *  `Iterable`.
   *
   *  @tparam A the element type of the collection
   *  @param c the Java `Collection` to convert
   */
  implicit class CollectionHasAsScala[A](c: ju.Collection[A]) {
    /** Converts a Java `Collection` to a Scala `Iterable`, see
      * [[AsScalaConverters.asScala[A](c:java\.util\.Collection[A])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: Iterable[A] = conv.asScala(c)
  }

  /** Provides the `asScala` extension method that converts a Java `List` to a Scala mutable
   *  `Buffer`.
   *
   *  The conversion returns a wrapper, not a copy: changes made through either interface are
   *  visible through the other. A list that was itself obtained by calling `asJava` on a Scala
   *  `Buffer` is unwrapped, returning that original `Buffer`.
   *
   *  @tparam A the element type of the list
   *  @param l the Java `List` to convert
   */
  implicit class ListHasAsScala[A](l: ju.List[A]) {
    /** Converts a Java `List` to a Scala `Buffer`, see
      * [[AsScalaConverters.asScala[A](l:java\.util\.List[A])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: mutable.Buffer[A] = conv.asScala(l)
  }

  /** Provides the `asScala` extension method that converts a Java `Set` to a Scala mutable
   *  `Set`.
   *
   *  The conversion returns a wrapper, not a copy: changes made through either interface are
   *  visible through the other. A set that was itself obtained by calling `asJava` on a Scala
   *  mutable `Set` is unwrapped, returning that original `Set`.
   *
   *  @tparam A the element type of the set
   *  @param s the Java `Set` to convert
   */
  implicit class SetHasAsScala[A](s: ju.Set[A]) {
    /** Converts a Java `Set` to a Scala `Set`, see
      * [[AsScalaConverters.asScala[A](s:java\.util\.Set[A])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: mutable.Set[A] = conv.asScala(s)
  }

  /** Provides the `asScala` extension method that converts a Java `Map` to a Scala mutable
   *  `Map`.
   *
   *  The conversion returns a wrapper, not a copy: changes made through either interface are
   *  visible through the other. A map that was itself obtained by calling `asJava` on a Scala
   *  mutable `Map` is unwrapped, returning that original `Map`.
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   *  @param m the Java `Map` to convert
   */
  implicit class MapHasAsScala[K, V](m: ju.Map[K, V]) {
    /** Converts a Java `Map` to a Scala `Map`, see
      * [[AsScalaConverters.asScala[A,B](m:java\.util\.Map[A,B])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: mutable.Map[K, V] = conv.asScala(m)
  }

  /** Provides the `asScala` extension method that converts a Java `ConcurrentMap` to a Scala
   *  `concurrent.Map`.
   *
   *  The conversion returns a wrapper, not a copy: changes made through either interface are
   *  visible through the other. A map that was itself obtained through `asJava` is unwrapped,
   *  returning the original Scala `concurrent.Map`.
   *
   *  @tparam K the key type of the map
   *  @tparam V the value type of the map
   *  @param m the Java `ConcurrentMap` to convert
   */
  implicit class ConcurrentMapHasAsScala[K, V](m: juc.ConcurrentMap[K, V]) {
    /** Converts a Java `ConcurrentMap` to a Scala `concurrent.Map`, see
      * [[AsScalaConverters.asScala[A,B](m:java\.util\.concurrent\.ConcurrentMap[A,B])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: concurrent.Map[K, V] = conv.asScala(m)
  }

  /** Provides the `asScala` extension method that converts a Java `Dictionary` to a Scala
   *  mutable `Map`.
   *
   *  The conversion returns a wrapper, not a copy: changes made through either interface are
   *  visible through the other. A dictionary that was itself obtained through
   *  `asJavaDictionary` is unwrapped, returning the original Scala `Map`.
   *
   *  @tparam K the key type of the dictionary
   *  @tparam V the value type of the dictionary
   *  @param d the Java `Dictionary` to convert
   */
  implicit class DictionaryHasAsScala[K, V](d: ju.Dictionary[K, V]) {
    /** Converts a Java `Dictionary` to a Scala `Map`, see
      * [[AsScalaConverters.asScala[A,B](d:java\.util\.Dictionary[A,B])* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: mutable.Map[K, V] = conv.asScala(d)
  }

  /** Provides the `asScala` extension method that converts a Java `Properties` to a Scala
   *  mutable `Map[String, String]`.
   *
   *  The conversion returns a wrapper, not a copy: changes made through either interface are
   *  visible through the other. This conversion is one-way; there is no corresponding `asJava`
   *  conversion to `Properties`, and the result is always a new wrapper.
   *
   *  The wrapper exposes only the `Properties` object's own entries; the defaults it may have
   *  been constructed with are not consulted. It also assumes every entry has a `String` key
   *  and value, so an entry of any other type can make its operations fail.
   *
   *  @param i the Java `Properties` to convert
   */
  implicit class PropertiesHasAsScala(i: ju.Properties) {
    /** Converts a Java `Properties` to a Scala `Map`, see
      * [[AsScalaConverters.asScala(p:java\.util\.Properties)* `scala.jdk.javaapi.CollectionConverters.asScala`]].
      */
    def asScala: mutable.Map[String, String] = conv.asScala(i)
  }
}
