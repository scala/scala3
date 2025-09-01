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

// DO NOT EDIT, CHANGES WILL BE LOST
// This auto-generated code can be modified in "project/GenerateAnyVals.scala".
// Afterwards, running "sbt generateSources" regenerates this source file.

package scala

import scala.language.`2.13`

/** `Unit` is a subtype of [[scala.AnyVal]]. There is only one value of type
 *  `Unit`, `()`, and it is not represented by any object in the underlying
 *  runtime system. A method with return type `Unit` is analogous to a Java
 *  method which is declared `void`.
 */
final abstract class Unit private extends AnyVal {
  // Provide a more specific return type for Scaladoc
  override def getClass(): Class[Unit] = ???
}

@scala.annotation.compileTimeOnly("`Unit` companion object is not allowed in source; instead, use `()` for the unit value")
object Unit extends AnyValCompanion {

  /** Transforms a value type into a boxed reference type.
   *
   *  This method is not intended for use in source code.
   *  The runtime representation of this value is platform specific.
   *
   *  @param  x   the Unit to be boxed
   *  @return     a scala.runtime.BoxedUnit offering `x` as its underlying value.
   */
  def box(x: Unit): scala.runtime.BoxedUnit = scala.runtime.BoxedUnit.UNIT

  /** Transforms a boxed type into a value type.  Note that this
   *  method is not typesafe: it accepts any Object, but will throw
   *  an exception if the argument is not a scala.runtime.BoxedUnit.
   *
   *  This method is not intended for use in source code.
   *  The result of successfully unboxing a value is `()`.
   *
   *  @param  x   the scala.runtime.BoxedUnit to be unboxed.
   *  @throws     ClassCastException  if the argument is not a scala.runtime.BoxedUnit
   *  @return     the Unit value ()
   */
  def unbox(x: java.lang.Object): Unit = x.asInstanceOf[scala.runtime.BoxedUnit]

  /** The String representation of the scala.Unit companion object. */
  override def toString = "object scala.Unit"
}

