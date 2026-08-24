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

package scala.runtime

import scala.language.`2.13`

// things that should be in `Statics`, but can't be yet for bincompat reasons
// TODO 3.T: move to `Statics`
private[scala] object PStatics {
  // `Int.MaxValue - 8` traditional soft limit to maximize compatibility with diverse JVMs
  // See https://stackoverflow.com/a/8381338 for example
  /** The largest array length that common JVMs reliably allow:
   *  `Int.MaxValue - 8`.
   *
   *  Although array lengths are `Int`s, some VMs reserve a few header words
   *  in an array, so requesting a length above this limit can throw
   *  `OutOfMemoryError` even when enough memory is available. Array-backed
   *  collections use this value as the cap when growing their backing arrays.
   */
  final val VM_MaxArraySize = 2147483639
}
