/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.collection.immutable

import org.junit.Assert.*
import org.junit.Test

import tools.ReflectUtil

class LongMapTest {

  @Test
  def `isEmpty O(1)`(): Unit = {
    val m = LongMap(1L -> (), 2L -> (), 3L -> ())
    ReflectUtil.getFieldAccessible[LongMap.Bin[?]]("left").set(m, null)
    ReflectUtil.getFieldAccessible[LongMap.Bin[?]]("right").set(m, null)
    assertFalse(m.isEmpty) // no NPE, does not access left or right
  }
}
