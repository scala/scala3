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

/** This class implements errors which are thrown whenever an
 *  object doesn't match any pattern of a pattern matching
 *  expression.
 */
final class MatchError(@transient obj: Any) extends RuntimeException {
  /** There's no reason we need to call toString eagerly,
   *  so defer it until getMessage is called or object is serialized
   */
  private[this] lazy val objString = {
    if (obj == null) "null"
    else {
      val cls = obj.getClass()
      val ofClass =
        if (cls == null) "of a JS class"
        else s"of class ${cls.getName()}"
      try s"$obj ($ofClass)"
      catch {
        case _: Throwable => "an instance " + ofClass
      }
    }
  }

  @throws[java.io.ObjectStreamException]
  private def writeReplace(): Object = {
    objString
    this
  }

  override def getMessage() = objString
}
