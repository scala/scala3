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

import java.lang.invoke._
import java.util

import scala.annotation.varargs
import scala.collection.immutable

import scala.language.`2.13`

/** The per-class state behind the synthetic `$deserializeLambda$` method of
 *  a class hosting lambdas: the class's lookup, a map from implementation
 *  method name-and-descriptor keys to their method handles, and a cache of
 *  deserialization factories keyed the same way.
 *
 *  Created by `LambdaDeserialize.bootstrap`, which the JVM invokes via
 *  `invokedynamic`.
 */
final class LambdaDeserialize private (lookup: MethodHandles.Lookup, targetMethods: Array[MethodHandle]) {
  private val targetMethodMap: util.HashMap[String, MethodHandle] = new util.HashMap[String, MethodHandle](targetMethods.length)

  for (targetMethod <- targetMethods) {
    val info = lookup.revealDirect(targetMethod)
    val key = LambdaDeserialize.nameAndDescriptorKey(info.getName, info.getMethodType.toMethodDescriptorString)
    targetMethodMap.put(key, targetMethod)
  }

  private val cache = new util.HashMap[String, MethodHandle]

  /** Returns an instance of the functional interface described by
   *  `serialized`, delegating to [[LambdaDeserializer.deserializeLambda]]
   *  with this instance's lookup, factory cache, and target method map.
   *
   *  @param serialized the serialized form of the lambda to deserialize
   *  @throws IllegalArgumentException if the implementation method named by
   *          `serialized` is not among this instance's target methods
   */
  def deserializeLambda(serialized: SerializedLambda): AnyRef = LambdaDeserializer.deserializeLambda(lookup, cache, targetMethodMap, serialized)
}

object LambdaDeserialize {
  /** Bootstrap method that the JVM invokes, via the `invokedynamic`
   *  instruction in the synthetic `$deserializeLambda$` method of a class
   *  hosting lambdas, to link that method's call site.
   *
   *  @param lookup the lookup of the class hosting the lambdas
   *  @param invokedName never used
   *  @param invokedType the type the call site's target is adapted to,
   *                     taking a `SerializedLambda` and returning the
   *                     deserialized object
   *  @param targetMethods handles for the lambda implementation methods of
   *                       the class, from which deserialization requests
   *                       are resolved by name and descriptor
   *  @return a `ConstantCallSite` whose target is `deserializeLambda` bound
   *          to a `LambdaDeserialize` built over `lookup` and
   *          `targetMethods`
   */
  @varargs @throws[Throwable]
  def bootstrap(lookup: MethodHandles.Lookup, invokedName: String, invokedType: MethodType, targetMethods: MethodHandle*): CallSite = {
    val targetMethodsArray = targetMethods.asInstanceOf[immutable.ArraySeq[?]].unsafeArray.asInstanceOf[Array[MethodHandle]]
    val exact = MethodHandleConstants.LAMBDA_DESERIALIZE_DESERIALIZE_LAMBDA.bindTo(new LambdaDeserialize(lookup, targetMethodsArray)).asType(invokedType)
    new ConstantCallSite(exact)
  }

  /** Returns the key under which an implementation method is stored in the
   *  target method map and factory cache: `name` concatenated with
   *  `descriptor`.
   *
   *  @param name the name of the implementation method
   *  @param descriptor the JVM method descriptor of its signature
   */
  def nameAndDescriptorKey(name: String, descriptor: String): String = name + descriptor
}
