/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package scala.tasty.file

object ConsumeTasty {

  /** Load and process TASTy files using TASTy reflect
   *
   * @param classpath Classpath where the classes are located
   * @param classes classes to be consumed
   * @param tastyConsumer consumer that will process the tasty trees
   */
  def apply(classpath: String, classes: List[String], tastyConsumer: TastyConsumer): Unit = {
    val cl = getClass.getClassLoader
    try {
      val dottyConsumeTastyCls = cl.loadClass("dotty.tools.dotc.consumetasty.ConsumeTasty")
      val makeMeth = dottyConsumeTastyCls.getMethod("apply", classOf[String], classOf[List[_]], classOf[TastyConsumer])
      makeMeth.invoke(null, classpath, classes, tastyConsumer)
    }
    catch {
      case ex: ClassNotFoundException =>
        throw new Exception(
          s"""Could not load the dotty.tools.dotc.consumetasty.ConsumeTasty class `${ex.getMessage}` from the JVM classpath. Make sure that the compiler is on the JVM classpath.""",
          ex
        )
    }
  }
}
