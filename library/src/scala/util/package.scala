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

import scala.language.`2.13`

import scala.annotation.nowarn

package object util {
  /**
   * Adds chaining methods `tap` and `pipe` to every type.
   */
  @nowarn("msg=ChainingSyntax will be removed in the future")
  object chaining extends ChainingSyntax:
    extension[A](x: A)
      inline def tap(inline f: A => Unit): x.type = { f(x); x }
      inline def pipe[B](inline f: A => B): B = f(x)
}
