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

import language.experimental.captureChecking
import scala.language.`2.13`
import scala.util.control.ControlThrowable

class NonLocalReturnControl[@specialized(Byte, Short, Int, Long, Char, Float, Double, Boolean/*, Unit -- see below*/) T](val key: AnyRef, val value: T) extends ControlThrowable {
  final override def fillInStackTrace(): Throwable = this
}

// This hacky class is a manual specialization of the above for binary compatibility with Scala 2,
// because Scala 2's specialization has odd behavior for Unit.
// This is what javap shows for the Scala2-compiled version:
// public class scala.runtime.NonLocalReturnControl$mcV$sp extends scala.runtime.NonLocalReturnControl<scala.runtime.BoxedUnit> {
//   public final scala.runtime.BoxedUnit value$mcV$sp;
//   public void value$mcV$sp();
//   public void value();
//   public boolean specInstance$();
//   public java.lang.Object value();
//   public scala.runtime.NonLocalReturnControl$mcV$sp(java.lang.Object, scala.runtime.BoxedUnit);
// }
// As one can tell, the inheritance is messed up since there is no proper inheritance for `value`.
// This class here has all of that, and in addition has `public scala.runtime.BoxedUnit value();` and `public scala.runtime.BoxedUnit value$mcV$sp();`.
class `NonLocalReturnControl$mcV$sp`(key: AnyRef, override val value: Unit) extends NonLocalReturnControl[Unit](key, ()) {
  @scala.annotation.targetName("value")
  def valueHack(): Unit = ()
  def `value$mcV$sp`(): Unit = ()
  @scala.annotation.targetName("value$mcV$sp")
  val valueHack2: Unit = ()
}