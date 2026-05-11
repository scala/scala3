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

package scala.jdk.javaapi

import scala.language.`2.13`
import java.util.{Optional, OptionalDouble, OptionalInt, OptionalLong}
import java.{lang => jl}

/** This object contains methods that convert between Scala `Option` and Java `Optional` types.
 *
 *  The explicit conversion methods defined here are intended to be used in Java code. For Scala
 *  code, it is recommended to use the extension methods defined in [[scala.jdk.OptionConverters]].
 *
 *  @define primitiveNote Note: this method uses the boxed type `java.lang.X` instead of the
 *                       primitive type `scala.X` to improve compatibility when using it in
 *                       Java code (the Scala compiler emits `C[Int]` as `C[Object]` in bytecode
 *                       due to [scala/bug#4214](https://github.com/scala/bug/issues/4214)). In
 *                       Scala code, add `import scala.jdk.OptionConverters._` and use the
 *                       extension methods instead.
 */
object OptionConverters {
  /** Converts a Scala `Option` to a Java `Optional`.
   *
   *  @tparam A the element type of the `Option` and the resulting `Optional`
   *  @param o the Scala `Option` to convert
   *  @return a Java `Optional` containing the value, or empty if `o` is `None`
   */
  def toJava[A](o: Option[A]): Optional[A] = o match {
    case Some(a) => Optional.ofNullable(a)
    case _ => Optional.empty[A]
  }

  /** Converts a Scala `Option[java.lang.Double]` to a Java `OptionalDouble`.
   *
   *  $primitiveNote
   *
   *  @param o the Scala `Option` to convert
   *  @return a Java `OptionalDouble` containing the value, or empty if `o` is `None`
   */
  def toJavaOptionalDouble(o: Option[jl.Double]): OptionalDouble = o match {
    case Some(a) => OptionalDouble.of(a)
    case _ => OptionalDouble.empty
  }

  /** Converts a Scala `Option[java.lang.Integer]` to a Java `OptionalInt`.
   *
   *  $primitiveNote
   *
   *  @param o the Scala `Option` to convert
   *  @return a Java `OptionalInt` containing the value, or empty if `o` is `None`
   */
  def toJavaOptionalInt(o: Option[jl.Integer]): OptionalInt = o match {
    case Some(a) => OptionalInt.of(a)
    case _ => OptionalInt.empty
  }

  /** Converts a Scala `Option[java.lang.Long]` to a Java `OptionalLong`.
   *
   *  $primitiveNote
   *
   *  @param o the Scala `Option` to convert
   *  @return a Java `OptionalLong` containing the value, or empty if `o` is `None`
   */
  def toJavaOptionalLong(o: Option[jl.Long]): OptionalLong = o match {
    case Some(a) => OptionalLong.of(a)
    case _ => OptionalLong.empty
  }

  /** Converts a Java `Optional` to a Scala `Option`.
   *
   *  @tparam A the element type of the `Optional`
   *  @param o the Java `Optional` to convert
   *  @return a Scala `Option` containing the value, or `None` if the `Optional` is empty
   */
  def toScala[A](o: Optional[A]): Option[A] = if (o.isPresent) Some(o.get) else None

  /** Converts a Java `OptionalDouble` to a Scala `Option[java.lang.Double]`.
   *
   *  $primitiveNote
   *
   *  @param o the Java `OptionalDouble` to convert
   *  @return a Scala `Option` containing the value as a boxed `java.lang.Double`, or `None` if empty
   */
  def toScala(o: OptionalDouble): Option[jl.Double] = if (o.isPresent) Some(o.getAsDouble) else None

  /** Converts a Java `OptionalInt` to a Scala `Option[java.lang.Integer]`.
   *
   *  $primitiveNote
   *
   *  @param o the Java `OptionalInt` to convert
   *  @return a Scala `Option` containing the value as a boxed `java.lang.Integer`, or `None` if empty
   */
  def toScala(o: OptionalInt): Option[jl.Integer] = if (o.isPresent) Some(o.getAsInt) else None

  /** Converts a Java `OptionalLong` to a Scala `Option[java.lang.Long]`.
   *
   *  $primitiveNote
   *
   *  @param o the Java `OptionalLong` to convert
   *  @return a Scala `Option` containing the value as a boxed `java.lang.Long`, or `None` if empty
   */
  def toScala(o: OptionalLong): Option[jl.Long] = if (o.isPresent) Some(o.getAsLong) else None
}
