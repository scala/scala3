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

// GENERATED CODE: DO NOT EDIT.


package scala.jdk

import scala.language.`2.13`

/** This object provides extension methods that convert between Scala and Java function types.
  *
  * When writing Java code, use the explicit conversion methods defined in
  * [[javaapi.FunctionConverters]] instead.
  *
  * Using the `.asJava` extension method on a Scala function produces the most specific possible
  * Java function type:
  *
  * ```scala sc:compile
  * import scala.jdk.FunctionConverters.*
  * val f = (x: Int) => x + 1
  * val jf1 = f.asJava
  * ```
  *
  * More generic Java function types can be created using the corresponding `asJavaXYZ` extension
  * method:
  *
  * ```scala sc:compile
  * import scala.jdk.FunctionConverters.*
  * val f = (x: Int) => x + 1
  * val jf2 = f.asJavaFunction
  * val jf3 = f.asJavaUnaryOperator
  * ```
  *
  * Converting a Java function to Scala is done using the `asScala` extension method:
  *
  * ```scala sc:compile
  * import scala.jdk.FunctionConverters.*
  * val f = (x: Int) => x + 1
  * val jf2 = f.asJavaFunction
  * val mapped = List(1, 2, 3).map(jf2.asScala)
  * assert(mapped == List(2, 3, 4))
  * ```
  */
object FunctionConverters extends Priority0FunctionExtensions
