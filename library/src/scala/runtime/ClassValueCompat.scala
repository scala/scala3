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
import scala.runtime.ClassValueCompat._

private[scala] abstract class ClassValueCompat[T] extends ClassValueInterface[T] { self =>
  private val instance: ClassValueInterface[T] =
    if (classValueAvailable) new JavaClassValue()
    else new FallbackClassValue()

  private class JavaClassValue extends ClassValue[T] with ClassValueInterface[T] {
    override def computeValue(cls: Class[?]): T = self.computeValue(cls)
  }

  private class FallbackClassValue extends ClassValueInterface[T] {
    override def get(cls: Class[?]): T = self.computeValue(cls)

    override def remove(cls: Class[?]): Unit = {}
  }

  def get(cls: Class[?]): T = instance.get(cls)

  def remove(cls: Class[?]): Unit = instance.remove(cls)

  protected def computeValue(cls: Class[?]): T
}

private[scala] object ClassValueCompat {
  trait ClassValueInterface[T] {
    def get(cls: Class[?]): T

    def remove(cls: Class[?]): Unit
  }

  private val classValueAvailable: Boolean = try {
    Class.forName("java.lang.ClassValue", false, classOf[Object].getClassLoader)
    true
  } catch {
    case _: ClassNotFoundException => false
  }
}
