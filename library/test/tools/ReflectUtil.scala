package tools

import java.lang.reflect.{Field, Modifier}
import scala.reflect.{ClassTag, classTag, ensureAccessible}
import scala.util.chaining.scalaUtilChainingOps

/** This module contains reflection-related utilities.
 *
 *  This object contains methods that will not work on Scala.js nor Scala
 *  Native, making any test using `ReflectUtil` JVM-only.
 */
object ReflectUtil {
  def getFieldAccessible[T: ClassTag](n: String): Field =
    classTag[T]
      .runtimeClass.getDeclaredField(n)
      .tap { f =>
        if ((f.getModifiers & Modifier.PUBLIC) == 0)
          f.setAccessible(true)
      }
}
