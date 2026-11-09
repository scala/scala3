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

package scala.collection.convert
package impl

import scala.language.`2.13`
import java.util.Spliterator

import scala.collection.Stepper.EfficientSplit

/** Abstracts all the generic operations of stepping over an indexable collection.
 *
 *  @tparam Sub the concrete stepper subtype
 *  @tparam Semi the type returned by splitting, a subtype of `Sub`
 *  @param i0 the starting index (inclusive) into the underlying collection
 *  @param iN the ending index (exclusive) into the underlying collection
 */
private[convert] abstract class IndexedStepperBase[Sub, Semi <: Sub](protected var i0: Int, protected var iN: Int)
  extends EfficientSplit {
  protected def semiclone(half: Int): Semi

  def hasStep: Boolean = i0 < iN

  def characteristics: Int = Spliterator.ORDERED + Spliterator.SIZED + Spliterator.SUBSIZED

  def estimateSize: Long = iN - i0

  def trySplit(): Sub | Null = {
    if (iN-1 > i0) {
      val half = (i0+iN) >>> 1
      val ans = semiclone(half)
      i0 = half
      ans
    }
    else null
  }
}
