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

import java.util.{concurrent => juc}
import java.{lang => jl, util => ju}

import scala.collection.JavaConverters._
import scala.language.implicitConversions

/** Defines implicit converter methods from Java to Scala collections. */
@deprecated("Use `scala.jdk.CollectionConverters` instead", "2.13.0")
trait ToScalaImplicits {
  /** Implicitly converts a Java `Iterator` to a Scala `Iterator`.
   *  @see [[JavaConverters.asScalaIterator]]
   */
  implicit def `iterator asScala`[A](it: ju.Iterator[A] | Null): Iterator[A] | Null = asScalaIterator(it)

  /** Implicitly converts a Java `Enumeration` to a Scala `Iterator`.
   *  @see [[JavaConverters.enumerationAsScalaIterator]]
   */
  implicit def `enumeration AsScalaIterator`[A](i: ju.Enumeration[A] | Null): Iterator[A] | Null = enumerationAsScalaIterator(i)

  /** Implicitly converts a Java `Iterable` to a Scala `Iterable`.
   *  @see [[JavaConverters.iterableAsScalaIterable]]
   */
  implicit def `iterable AsScalaIterable`[A](i: jl.Iterable[A] | Null): Iterable[A] | Null = iterableAsScalaIterable(i)

  /** Implicitly converts a Java `Collection` to an Scala `Iterable`.
   *  @see [[JavaConverters.collectionAsScalaIterable]]
   */
  implicit def `collection AsScalaIterable`[A](i: ju.Collection[A] | Null): Iterable[A] | Null = collectionAsScalaIterable(i)

  /** Implicitly converts a Java `List` to a Scala mutable `Buffer`.
   *  @see [[JavaConverters.asScalaBuffer]]
   */
  implicit def `list asScalaBuffer`[A](l: ju.List[A] | Null): mutable.Buffer[A] | Null = asScalaBuffer(l)

  /** Implicitly converts a Java `Set` to a Scala mutable `Set`.
   *  @see [[JavaConverters.asScalaSet]]
   */
  implicit def `set asScala`[A](s: ju.Set[A] | Null): mutable.Set[A] | Null = asScalaSet(s)

  /** Implicitly converts a Java `Map` to a Scala mutable `Map`.
   *  @see [[JavaConverters.mapAsScalaMap]]
   */
  implicit def `map AsScala`[K, V](m: ju.Map[K, V] | Null): mutable.Map[K, V] | Null = mapAsScalaMap(m)

  /** Implicitly converts a Java `ConcurrentMap` to a Scala mutable `ConcurrentMap`.
   *  @see [[JavaConverters.mapAsScalaConcurrentMap]]
   */
  implicit def `map AsScalaConcurrentMap`[K, V](m: juc.ConcurrentMap[K, V] | Null): concurrent.Map[K, V] | Null = mapAsScalaConcurrentMap(m)

  /** Implicitly converts a Java `Dictionary` to a Scala mutable `Map`.
   *  @see [[JavaConverters.dictionaryAsScalaMap]]
   */
  implicit def `dictionary AsScalaMap`[K, V](p: ju.Dictionary[K, V] | Null): mutable.Map[K, V] | Null = dictionaryAsScalaMap(p)

  /** Implicitly converts a Java `Properties` to a Scala `mutable Map[String, String]`.
   *  @see [[JavaConverters.propertiesAsScalaMap]]
   */
  implicit def `properties AsScalaMap`(p: ju.Properties | Null): mutable.Map[String, String] | Null = propertiesAsScalaMap(p)
}

/** Defines implicit conversions from Scala to Java collections. */
@deprecated("Use `scala.jdk.CollectionConverters` instead", "2.13.0")
trait ToJavaImplicits {
  /** Implicitly converts a Scala `Iterator` to a Java `Iterator`.
   *  @see [[JavaConverters.asJavaIterator]]
   */
  implicit def `iterator asJava`[A](it: Iterator[A] | Null): ju.Iterator[A] | Null = asJavaIterator(it)

  /** Implicitly converts a Scala `Iterator` to a Java `Enumeration`.
   *  @see [[JavaConverters.asJavaEnumeration]]
   */
  implicit def `enumeration asJava`[A](it: Iterator[A] | Null): ju.Enumeration[A] | Null = asJavaEnumeration(it)

  /** Implicitly converts a Scala `Iterable` to a Java `Iterable`.
   *  @see [[JavaConverters.asJavaIterable]]
   */
  implicit def `iterable asJava`[A](i: Iterable[A] | Null): jl.Iterable[A] | Null = asJavaIterable(i)

  /** Implicitly converts a Scala `Iterable` to an immutable Java `Collection`.
   *  @see [[JavaConverters.asJavaCollection]]
   */
  implicit def `collection asJava`[A](it: Iterable[A] | Null): ju.Collection[A] | Null = asJavaCollection(it) 

  /** Implicitly converts a Scala mutable `Buffer` to a Java `List`.
   *  @see [[JavaConverters.bufferAsJavaList]]
   */
  implicit def `buffer AsJavaList`[A](b: mutable.Buffer[A] | Null): ju.List[A] | Null = bufferAsJavaList(b)

  /** Implicitly converts a Scala mutable `Seq` to a Java `List`.
   *  @see [[JavaConverters.mutableSeqAsJavaList]]
   */
  implicit def `mutableSeq AsJavaList`[A](seq: mutable.Seq[A] | Null): ju.List[A] | Null = mutableSeqAsJavaList(seq)

  /** Implicitly converts a Scala `Seq` to a Java `List`.
   *  @see [[JavaConverters.seqAsJavaList]]
   */
  implicit def `seq AsJavaList`[A](seq: Seq[A] | Null): ju.List[A] | Null = seqAsJavaList(seq)

  /** Implicitly converts a Scala mutable `Set` to a Java `Set`.
   *  @see [[JavaConverters.mutableSetAsJavaSet]]
   */
  implicit def `mutableSet AsJavaSet`[A](s: mutable.Set[A] | Null): ju.Set[A] | Null = mutableSetAsJavaSet(s)

  /** Implicitly converts a Scala `Set` to a Java `Set`.
   *  @see [[JavaConverters.setAsJavaSet]]
   */
  implicit def `set AsJavaSet`[A](s: Set[A] | Null): ju.Set[A] | Null = setAsJavaSet(s)

  /** Implicitly converts a Scala mutable `Map` to a Java `Map`.
   *  @see [[JavaConverters.mutableMapAsJavaMap]]
   */
  implicit def `mutableMap AsJavaMap`[K, V](m: mutable.Map[K, V] | Null): ju.Map[K, V] | Null = mutableMapAsJavaMap(m)

  /** Implicitly converts a Scala mutable `Map` to a Java `Dictionary`.
   *  @see [[JavaConverters.asJavaDictionary]]
   */
  implicit def `dictionary asJava`[K, V](m: mutable.Map[K, V] | Null): ju.Dictionary[K, V] | Null = asJavaDictionary(m)

  /** Implicitly converts a Scala `Map` to a Java `Map`.
   *  @see [[JavaConverters.mapAsJavaMap]]
   */
  implicit def `map AsJavaMap`[K, V](m: Map[K, V] | Null): ju.Map[K, V] | Null = mapAsJavaMap(m)

  /** Implicitly converts a Scala mutable `concurrent.Map` to a Java `ConcurrentMap`.
   *  @see [[JavaConverters.mapAsJavaConcurrentMap]]
   */
  implicit def `map AsJavaConcurrentMap`[K, V](m: concurrent.Map[K, V] | Null): juc.ConcurrentMap[K, V] | Null = mapAsJavaConcurrentMap(m)
}

/**
 * Convenience for miscellaneous implicit conversions from Scala to Java collections API.
 *
 * It is recommended to use explicit conversions provided by [[collection.JavaConverters]] instead.
 * Implicit conversions may cause unexpected issues, see [[ImplicitConversions]].
 */
@deprecated("Use `scala.jdk.CollectionConverters` instead", "2.13.0")
object ImplicitConversionsToJava extends ToJavaImplicits

/**
 * Convenience for miscellaneous implicit conversions from Java to Scala collections API.
 *
 * It is recommended to use explicit conversions provided by [[collection.JavaConverters]] instead.
 * Implicit conversions may cause unexpected issues, see [[ImplicitConversions]].
 */
@deprecated("Use `scala.jdk.CollectionConverters` instead", "2.13.0")
object ImplicitConversionsToScala extends ToScalaImplicits

/**
 * Convenience for miscellaneous implicit conversions between Java and Scala collections API.
 *
 * It is recommended to use explicit conversions provided by [[collection.JavaConverters]] instead.
 * Implicit conversions may cause unexpected issues. Example:
 *
 * {{{
 *   import collection.convert.ImplicitConversions._
 *   case class StringBox(s: String)
 *   val m = Map(StringBox("one") -> "uno")
 *   m.get("one")
 * }}}
 *
 * The above example returns `null` instead of producing a type error at compile-time. The map is
 * implicitly converted to a `java.util.Map` which provides a method `get(x: AnyRef)`.
 */
@deprecated("Use `scala.jdk.CollectionConverters` instead", "2.13.0")
object ImplicitConversions extends ToScalaImplicits with ToJavaImplicits
