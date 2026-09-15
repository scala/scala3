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

package scala.collection.immutable

import scala.language.`2.13`
import language.experimental.captureChecking

import scala.collection.{IterableFactory, IterableFactoryDefaults}

/** A trait for collections that are guaranteed immutable.
  *
  * @tparam A the element type of the collection
  *
  * @define coll immutable collection
  * @define Coll `immutable.Iterable`
  */
trait Iterable[+A] extends collection.Iterable[A]
                      with collection.IterableOps[A, Iterable, Iterable[A]]
                      with IterableFactoryDefaults[A, Iterable] {

  /** The factory used to build immutable collections, the [[Iterable$ `Iterable`]] companion object, which delegates to [[List]]. */
  override def iterableFactory: IterableFactory[Iterable] = Iterable
}

@SerialVersionUID(3L)
object Iterable extends IterableFactory.Delegate[Iterable](List) {
  /** Returns an immutable collection containing the elements of `it`.
   *
   *  If `it` is already an immutable `Iterable` it is returned unchanged; otherwise
   *  its elements are copied into a new [[List]].
   *
   *  @tparam E the element type
   *  @param it the collection whose elements are to be contained
   */
  override def from[E](it: IterableOnce[E]^): Iterable[E]^{it} = it match {
    case iterable: Iterable[E @unchecked] => iterable
    case _ => super.from(it)
  }
}
