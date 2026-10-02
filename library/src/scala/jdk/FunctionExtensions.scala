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
import language.implicitConversions

/** Provides the lowest-priority implicit conversion from a Scala function to a Java functional
 *  interface wrapper. It applies only when no conversion declared in
 *  [[Priority2FunctionExtensions]] or one of its subtraits does, so that a Scala function is
 *  enriched with the most specific Java function type that fits it.
 */
trait Priority3FunctionExtensions {
  import FunctionWrappers._
  
  /** Enriches a Scala `Function2` with `asJava` and `asJavaBiFunction` methods that convert it to a `java.util.function.BiFunction`.
   *
   *  @tparam T the type of the first argument of `sf`
   *  @tparam U the type of the second argument of `sf`
   *  @tparam R the result type of `sf`
   *  @param sf the Scala function to convert
   *  @return a [[FunctionWrappers.RichFunction2AsBiFunction]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaBiFunction[T, U, R](sf: scala.Function2[T, U, R]): RichFunction2AsBiFunction[T, U, R] = new RichFunction2AsBiFunction[T, U, R](sf)
}



import language.implicitConversions

/** Provides implicit conversions from Scala functions to Java functional interface wrappers,
 *  taking precedence over the conversion inherited from [[Priority3FunctionExtensions]].
 */
trait Priority2FunctionExtensions extends Priority3FunctionExtensions {
  import FunctionWrappers._
  
  /** Enriches a Scala `Function2` with `asJava` and `asJavaBiConsumer` methods that convert it to a `java.util.function.BiConsumer`.
   *
   *  @tparam T the type of the first argument of `sf`
   *  @tparam U the type of the second argument of `sf`
   *  @param sf the Scala function to convert
   *  @return a [[FunctionWrappers.RichFunction2AsBiConsumer]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaBiConsumer[T, U](sf: scala.Function2[T, U, Unit]): RichFunction2AsBiConsumer[T, U] = new RichFunction2AsBiConsumer[T, U](sf)
  
  /** Enriches a Scala `Function2` with `asJava` and `asJavaBiPredicate` methods that convert it to a `java.util.function.BiPredicate`.
   *
   *  @tparam T the type of the first argument of `sf`
   *  @tparam U the type of the second argument of `sf`
   *  @param sf the Scala function to convert
   *  @return a [[FunctionWrappers.RichFunction2AsBiPredicate]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaBiPredicate[T, U](sf: scala.Function2[T, U, Boolean]): RichFunction2AsBiPredicate[T, U] = new RichFunction2AsBiPredicate[T, U](sf)
  
  /** Enriches a Scala `Function1` with `asJava` and `asJavaFunction` methods that convert it to a `java.util.function.Function`.
   *
   *  @tparam T the argument type of `sf`
   *  @tparam R the result type of `sf`
   *  @param sf the Scala function to convert
   *  @return a [[FunctionWrappers.RichFunction1AsFunction]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaFunction[T, R](sf: scala.Function1[T, R]): RichFunction1AsFunction[T, R] = new RichFunction1AsFunction[T, R](sf)
  
  /** Enriches a Scala `Function2` with `asJava` and `asJavaToDoubleBiFunction` methods that convert it to a `java.util.function.ToDoubleBiFunction`.
   *
   *  @tparam T the type of the first argument of `sf`
   *  @tparam U the type of the second argument of `sf`
   *  @param sf the Scala function to convert
   *  @return a [[FunctionWrappers.RichFunction2AsToDoubleBiFunction]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaToDoubleBiFunction[T, U](sf: scala.Function2[T, U, Double]): RichFunction2AsToDoubleBiFunction[T, U] = new RichFunction2AsToDoubleBiFunction[T, U](sf)
  
  /** Enriches a Scala `Function2` with `asJava` and `asJavaToIntBiFunction` methods that convert it to a `java.util.function.ToIntBiFunction`.
   *
   *  @tparam T the type of the first argument of `sf`
   *  @tparam U the type of the second argument of `sf`
   *  @param sf the Scala function to convert
   *  @return a [[FunctionWrappers.RichFunction2AsToIntBiFunction]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaToIntBiFunction[T, U](sf: scala.Function2[T, U, Int]): RichFunction2AsToIntBiFunction[T, U] = new RichFunction2AsToIntBiFunction[T, U](sf)
  
  /** Enriches a Scala `Function2` with `asJava` and `asJavaToLongBiFunction` methods that convert it to a `java.util.function.ToLongBiFunction`.
   *
   *  @tparam T the type of the first argument of `sf`
   *  @tparam U the type of the second argument of `sf`
   *  @param sf the Scala function to convert
   *  @return a [[FunctionWrappers.RichFunction2AsToLongBiFunction]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaToLongBiFunction[T, U](sf: scala.Function2[T, U, Long]): RichFunction2AsToLongBiFunction[T, U] = new RichFunction2AsToLongBiFunction[T, U](sf)
}



import language.implicitConversions

/** Provides implicit conversions from Scala functions to Java functional interface wrappers,
 *  taking precedence over those inherited from [[Priority2FunctionExtensions]].
 */
trait Priority1FunctionExtensions extends Priority2FunctionExtensions {
  import FunctionWrappers._
  
  /** Enriches a Scala `Function2` with `asJava` and `asJavaBinaryOperator` methods that convert it to a `java.util.function.BinaryOperator`.
   *
   *  @tparam T the type of the first argument of `sf`
   *  @tparam A1 the type of the second argument of `sf`, constrained by `evA1` to be `T`
   *  @tparam A2 the result type of `sf`, constrained by `evA2` to be `T`
   *  @param sf the Scala function to convert
   *  @param evA1 evidence that `A1` is `T`
   *  @param evA2 evidence that `A2` is `T`
   *  @return a [[FunctionWrappers.RichFunction2AsBinaryOperator]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaBinaryOperator[T, A1, A2](sf: scala.Function2[T, A1, A2])(implicit evA1: =:=[A1, T], evA2: =:=[A2, T]): RichFunction2AsBinaryOperator[T] = new RichFunction2AsBinaryOperator[T](sf.asInstanceOf[scala.Function2[T, T, T]])
  
  /** Enriches a Scala `Function1` with `asJava` and `asJavaConsumer` methods that convert it to a `java.util.function.Consumer`.
   *
   *  @tparam T the argument type of `sf`
   *  @param sf the Scala function to convert
   *  @return a [[FunctionWrappers.RichFunction1AsConsumer]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaConsumer[T](sf: scala.Function1[T, Unit]): RichFunction1AsConsumer[T] = new RichFunction1AsConsumer[T](sf)
  
  /** Enriches a Scala `Function1` with `asJava` and `asJavaDoubleFunction` methods that convert it to a `java.util.function.DoubleFunction`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Double`
   *  @tparam R the result type of `sf`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Double`
   *  @return a [[FunctionWrappers.RichFunction1AsDoubleFunction]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaDoubleFunction[A0, R](sf: scala.Function1[A0, R])(implicit evA0: =:=[A0, Double]): RichFunction1AsDoubleFunction[R] = new RichFunction1AsDoubleFunction[R](sf.asInstanceOf[scala.Function1[Double, R]])
  
  /** Enriches a Scala `Function1` with `asJava` and `asJavaIntFunction` methods that convert it to a `java.util.function.IntFunction`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Int`
   *  @tparam R the result type of `sf`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Int`
   *  @return a [[FunctionWrappers.RichFunction1AsIntFunction]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaIntFunction[A0, R](sf: scala.Function1[A0, R])(implicit evA0: =:=[A0, Int]): RichFunction1AsIntFunction[R] = new RichFunction1AsIntFunction[R](sf.asInstanceOf[scala.Function1[Int, R]])
  
  /** Enriches a Scala `Function1` with `asJava` and `asJavaLongFunction` methods that convert it to a `java.util.function.LongFunction`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Long`
   *  @tparam R the result type of `sf`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Long`
   *  @return a [[FunctionWrappers.RichFunction1AsLongFunction]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaLongFunction[A0, R](sf: scala.Function1[A0, R])(implicit evA0: =:=[A0, Long]): RichFunction1AsLongFunction[R] = new RichFunction1AsLongFunction[R](sf.asInstanceOf[scala.Function1[Long, R]])
  
  /** Enriches a Scala `Function2` with `asJava` and `asJavaObjDoubleConsumer` methods that convert it to a `java.util.function.ObjDoubleConsumer`.
   *
   *  @tparam T the type of the first argument of `sf`
   *  @tparam A1 the type of the second argument of `sf`, constrained by `evA1` to be `Double`
   *  @param sf the Scala function to convert
   *  @param evA1 evidence that `A1` is `Double`
   *  @return a [[FunctionWrappers.RichFunction2AsObjDoubleConsumer]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaObjDoubleConsumer[T, A1](sf: scala.Function2[T, A1, Unit])(implicit evA1: =:=[A1, Double]): RichFunction2AsObjDoubleConsumer[T] = new RichFunction2AsObjDoubleConsumer[T](sf.asInstanceOf[scala.Function2[T, Double, Unit]])
  
  /** Enriches a Scala `Function2` with `asJava` and `asJavaObjIntConsumer` methods that convert it to a `java.util.function.ObjIntConsumer`.
   *
   *  @tparam T the type of the first argument of `sf`
   *  @tparam A1 the type of the second argument of `sf`, constrained by `evA1` to be `Int`
   *  @param sf the Scala function to convert
   *  @param evA1 evidence that `A1` is `Int`
   *  @return a [[FunctionWrappers.RichFunction2AsObjIntConsumer]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaObjIntConsumer[T, A1](sf: scala.Function2[T, A1, Unit])(implicit evA1: =:=[A1, Int]): RichFunction2AsObjIntConsumer[T] = new RichFunction2AsObjIntConsumer[T](sf.asInstanceOf[scala.Function2[T, Int, Unit]])
  
  /** Enriches a Scala `Function2` with `asJava` and `asJavaObjLongConsumer` methods that convert it to a `java.util.function.ObjLongConsumer`.
   *
   *  @tparam T the type of the first argument of `sf`
   *  @tparam A1 the type of the second argument of `sf`, constrained by `evA1` to be `Long`
   *  @param sf the Scala function to convert
   *  @param evA1 evidence that `A1` is `Long`
   *  @return a [[FunctionWrappers.RichFunction2AsObjLongConsumer]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaObjLongConsumer[T, A1](sf: scala.Function2[T, A1, Unit])(implicit evA1: =:=[A1, Long]): RichFunction2AsObjLongConsumer[T] = new RichFunction2AsObjLongConsumer[T](sf.asInstanceOf[scala.Function2[T, Long, Unit]])
  
  /** Enriches a Scala `Function1` with `asJava` and `asJavaPredicate` methods that convert it to a `java.util.function.Predicate`.
   *
   *  @tparam T the argument type of `sf`
   *  @param sf the Scala function to convert
   *  @return a [[FunctionWrappers.RichFunction1AsPredicate]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaPredicate[T](sf: scala.Function1[T, Boolean]): RichFunction1AsPredicate[T] = new RichFunction1AsPredicate[T](sf)
  
  /** Enriches a Scala `Function0` with `asJava` and `asJavaSupplier` methods that convert it to a `java.util.function.Supplier`.
   *
   *  @tparam T the result type of `sf`
   *  @param sf the Scala function to convert
   *  @return a [[FunctionWrappers.RichFunction0AsSupplier]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaSupplier[T](sf: scala.Function0[T]): RichFunction0AsSupplier[T] = new RichFunction0AsSupplier[T](sf)
  
  /** Enriches a Scala `Function1` with `asJava` and `asJavaToDoubleFunction` methods that convert it to a `java.util.function.ToDoubleFunction`.
   *
   *  @tparam T the argument type of `sf`
   *  @param sf the Scala function to convert
   *  @return a [[FunctionWrappers.RichFunction1AsToDoubleFunction]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaToDoubleFunction[T](sf: scala.Function1[T, Double]): RichFunction1AsToDoubleFunction[T] = new RichFunction1AsToDoubleFunction[T](sf)
  
  /** Enriches a Scala `Function1` with `asJava` and `asJavaToIntFunction` methods that convert it to a `java.util.function.ToIntFunction`.
   *
   *  @tparam T the argument type of `sf`
   *  @param sf the Scala function to convert
   *  @return a [[FunctionWrappers.RichFunction1AsToIntFunction]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaToIntFunction[T](sf: scala.Function1[T, Int]): RichFunction1AsToIntFunction[T] = new RichFunction1AsToIntFunction[T](sf)
  
  /** Enriches a Scala `Function1` with `asJava` and `asJavaToLongFunction` methods that convert it to a `java.util.function.ToLongFunction`.
   *
   *  @tparam T the argument type of `sf`
   *  @param sf the Scala function to convert
   *  @return a [[FunctionWrappers.RichFunction1AsToLongFunction]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaToLongFunction[T](sf: scala.Function1[T, Long]): RichFunction1AsToLongFunction[T] = new RichFunction1AsToLongFunction[T](sf)
  
  /** Enriches a Scala `Function1` with `asJava` and `asJavaUnaryOperator` methods that convert it to a `java.util.function.UnaryOperator`.
   *
   *  @tparam T the argument type of `sf`
   *  @tparam A1 the result type of `sf`, constrained by `evA1` to be `T`
   *  @param sf the Scala function to convert
   *  @param evA1 evidence that `A1` is `T`
   *  @return a [[FunctionWrappers.RichFunction1AsUnaryOperator]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaUnaryOperator[T, A1](sf: scala.Function1[T, A1])(implicit evA1: =:=[A1, T]): RichFunction1AsUnaryOperator[T] = new RichFunction1AsUnaryOperator[T](sf.asInstanceOf[scala.Function1[T, T]])
}



import language.implicitConversions

/** Provides the highest-priority implicit conversions between Scala functions and Java functional
 *  interfaces, taking precedence over those inherited from [[Priority1FunctionExtensions]].
 *  Alongside the conversions to the primitive-specialized Java function types, every conversion
 *  that enriches a Java functional interface with an `asScala` method is declared here.
 *  [[FunctionConverters]] extends this trait to make the whole hierarchy available at once.
 */
trait Priority0FunctionExtensions extends Priority1FunctionExtensions {
  import FunctionWrappers._
  
  /** Enriches a Scala `Function0` with an `asJava` method that converts it to a `java.util.function.BooleanSupplier`.
   *
   *  @param sf the Scala function to convert
   *  @return a [[FunctionWrappers.RichFunction0AsBooleanSupplier]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaBooleanSupplier(sf: scala.Function0[Boolean]): RichFunction0AsBooleanSupplier = new RichFunction0AsBooleanSupplier(sf)
  
  /** Enriches a Scala `Function2` with an `asJava` method that converts it to a `java.util.function.DoubleBinaryOperator`.
   *
   *  @tparam A0 the type of the first argument of `sf`, constrained by `evA0` to be `Double`
   *  @tparam A1 the type of the second argument of `sf`, constrained by `evA1` to be `Double`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Double`
   *  @param evA1 evidence that `A1` is `Double`
   *  @return a [[FunctionWrappers.RichFunction2AsDoubleBinaryOperator]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaDoubleBinaryOperator[A0, A1](sf: scala.Function2[A0, A1, Double])(implicit evA0: =:=[A0, Double], evA1: =:=[A1, Double]): RichFunction2AsDoubleBinaryOperator = new RichFunction2AsDoubleBinaryOperator(sf.asInstanceOf[scala.Function2[Double, Double, Double]])
  
  /** Enriches a Scala `Function1` with an `asJava` method that converts it to a `java.util.function.DoubleConsumer`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Double`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Double`
   *  @return a [[FunctionWrappers.RichFunction1AsDoubleConsumer]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaDoubleConsumer[A0](sf: scala.Function1[A0, Unit])(implicit evA0: =:=[A0, Double]): RichFunction1AsDoubleConsumer = new RichFunction1AsDoubleConsumer(sf.asInstanceOf[scala.Function1[Double, Unit]])
  
  /** Enriches a Scala `Function1` with an `asJava` method that converts it to a `java.util.function.DoublePredicate`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Double`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Double`
   *  @return a [[FunctionWrappers.RichFunction1AsDoublePredicate]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaDoublePredicate[A0](sf: scala.Function1[A0, Boolean])(implicit evA0: =:=[A0, Double]): RichFunction1AsDoublePredicate = new RichFunction1AsDoublePredicate(sf.asInstanceOf[scala.Function1[Double, Boolean]])
  
  /** Enriches a Scala `Function0` with an `asJava` method that converts it to a `java.util.function.DoubleSupplier`.
   *
   *  @param sf the Scala function to convert
   *  @return a [[FunctionWrappers.RichFunction0AsDoubleSupplier]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaDoubleSupplier(sf: scala.Function0[Double]): RichFunction0AsDoubleSupplier = new RichFunction0AsDoubleSupplier(sf)
  
  /** Enriches a Scala `Function1` with an `asJava` method that converts it to a `java.util.function.DoubleToIntFunction`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Double`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Double`
   *  @return a [[FunctionWrappers.RichFunction1AsDoubleToIntFunction]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaDoubleToIntFunction[A0](sf: scala.Function1[A0, Int])(implicit evA0: =:=[A0, Double]): RichFunction1AsDoubleToIntFunction = new RichFunction1AsDoubleToIntFunction(sf.asInstanceOf[scala.Function1[Double, Int]])
  
  /** Enriches a Scala `Function1` with an `asJava` method that converts it to a `java.util.function.DoubleToLongFunction`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Double`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Double`
   *  @return a [[FunctionWrappers.RichFunction1AsDoubleToLongFunction]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaDoubleToLongFunction[A0](sf: scala.Function1[A0, Long])(implicit evA0: =:=[A0, Double]): RichFunction1AsDoubleToLongFunction = new RichFunction1AsDoubleToLongFunction(sf.asInstanceOf[scala.Function1[Double, Long]])
  
  /** Enriches a Scala `Function1` with an `asJava` method that converts it to a `java.util.function.DoubleUnaryOperator`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Double`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Double`
   *  @return a [[FunctionWrappers.RichFunction1AsDoubleUnaryOperator]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaDoubleUnaryOperator[A0](sf: scala.Function1[A0, Double])(implicit evA0: =:=[A0, Double]): RichFunction1AsDoubleUnaryOperator = new RichFunction1AsDoubleUnaryOperator(sf.asInstanceOf[scala.Function1[Double, Double]])
  
  /** Enriches a Scala `Function2` with an `asJava` method that converts it to a `java.util.function.IntBinaryOperator`.
   *
   *  @tparam A0 the type of the first argument of `sf`, constrained by `evA0` to be `Int`
   *  @tparam A1 the type of the second argument of `sf`, constrained by `evA1` to be `Int`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Int`
   *  @param evA1 evidence that `A1` is `Int`
   *  @return a [[FunctionWrappers.RichFunction2AsIntBinaryOperator]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaIntBinaryOperator[A0, A1](sf: scala.Function2[A0, A1, Int])(implicit evA0: =:=[A0, Int], evA1: =:=[A1, Int]): RichFunction2AsIntBinaryOperator = new RichFunction2AsIntBinaryOperator(sf.asInstanceOf[scala.Function2[Int, Int, Int]])
  
  /** Enriches a Scala `Function1` with an `asJava` method that converts it to a `java.util.function.IntConsumer`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Int`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Int`
   *  @return a [[FunctionWrappers.RichFunction1AsIntConsumer]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaIntConsumer[A0](sf: scala.Function1[A0, Unit])(implicit evA0: =:=[A0, Int]): RichFunction1AsIntConsumer = new RichFunction1AsIntConsumer(sf.asInstanceOf[scala.Function1[Int, Unit]])
  
  /** Enriches a Scala `Function1` with an `asJava` method that converts it to a `java.util.function.IntPredicate`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Int`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Int`
   *  @return a [[FunctionWrappers.RichFunction1AsIntPredicate]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaIntPredicate[A0](sf: scala.Function1[A0, Boolean])(implicit evA0: =:=[A0, Int]): RichFunction1AsIntPredicate = new RichFunction1AsIntPredicate(sf.asInstanceOf[scala.Function1[Int, Boolean]])
  
  /** Enriches a Scala `Function0` with an `asJava` method that converts it to a `java.util.function.IntSupplier`.
   *
   *  @param sf the Scala function to convert
   *  @return a [[FunctionWrappers.RichFunction0AsIntSupplier]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaIntSupplier(sf: scala.Function0[Int]): RichFunction0AsIntSupplier = new RichFunction0AsIntSupplier(sf)
  
  /** Enriches a Scala `Function1` with an `asJava` method that converts it to a `java.util.function.IntToDoubleFunction`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Int`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Int`
   *  @return a [[FunctionWrappers.RichFunction1AsIntToDoubleFunction]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaIntToDoubleFunction[A0](sf: scala.Function1[A0, Double])(implicit evA0: =:=[A0, Int]): RichFunction1AsIntToDoubleFunction = new RichFunction1AsIntToDoubleFunction(sf.asInstanceOf[scala.Function1[Int, Double]])
  
  /** Enriches a Scala `Function1` with an `asJava` method that converts it to a `java.util.function.IntToLongFunction`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Int`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Int`
   *  @return a [[FunctionWrappers.RichFunction1AsIntToLongFunction]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaIntToLongFunction[A0](sf: scala.Function1[A0, Long])(implicit evA0: =:=[A0, Int]): RichFunction1AsIntToLongFunction = new RichFunction1AsIntToLongFunction(sf.asInstanceOf[scala.Function1[Int, Long]])
  
  /** Enriches a Scala `Function1` with an `asJava` method that converts it to a `java.util.function.IntUnaryOperator`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Int`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Int`
   *  @return a [[FunctionWrappers.RichFunction1AsIntUnaryOperator]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaIntUnaryOperator[A0](sf: scala.Function1[A0, Int])(implicit evA0: =:=[A0, Int]): RichFunction1AsIntUnaryOperator = new RichFunction1AsIntUnaryOperator(sf.asInstanceOf[scala.Function1[Int, Int]])
  
  /** Enriches a Scala `Function2` with an `asJava` method that converts it to a `java.util.function.LongBinaryOperator`.
   *
   *  @tparam A0 the type of the first argument of `sf`, constrained by `evA0` to be `Long`
   *  @tparam A1 the type of the second argument of `sf`, constrained by `evA1` to be `Long`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Long`
   *  @param evA1 evidence that `A1` is `Long`
   *  @return a [[FunctionWrappers.RichFunction2AsLongBinaryOperator]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaLongBinaryOperator[A0, A1](sf: scala.Function2[A0, A1, Long])(implicit evA0: =:=[A0, Long], evA1: =:=[A1, Long]): RichFunction2AsLongBinaryOperator = new RichFunction2AsLongBinaryOperator(sf.asInstanceOf[scala.Function2[Long, Long, Long]])
  
  /** Enriches a Scala `Function1` with an `asJava` method that converts it to a `java.util.function.LongConsumer`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Long`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Long`
   *  @return a [[FunctionWrappers.RichFunction1AsLongConsumer]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaLongConsumer[A0](sf: scala.Function1[A0, Unit])(implicit evA0: =:=[A0, Long]): RichFunction1AsLongConsumer = new RichFunction1AsLongConsumer(sf.asInstanceOf[scala.Function1[Long, Unit]])
  
  /** Enriches a Scala `Function1` with an `asJava` method that converts it to a `java.util.function.LongPredicate`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Long`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Long`
   *  @return a [[FunctionWrappers.RichFunction1AsLongPredicate]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaLongPredicate[A0](sf: scala.Function1[A0, Boolean])(implicit evA0: =:=[A0, Long]): RichFunction1AsLongPredicate = new RichFunction1AsLongPredicate(sf.asInstanceOf[scala.Function1[Long, Boolean]])
  
  /** Enriches a Scala `Function0` with an `asJava` method that converts it to a `java.util.function.LongSupplier`.
   *
   *  @param sf the Scala function to convert
   *  @return a [[FunctionWrappers.RichFunction0AsLongSupplier]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaLongSupplier(sf: scala.Function0[Long]): RichFunction0AsLongSupplier = new RichFunction0AsLongSupplier(sf)
  
  /** Enriches a Scala `Function1` with an `asJava` method that converts it to a `java.util.function.LongToDoubleFunction`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Long`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Long`
   *  @return a [[FunctionWrappers.RichFunction1AsLongToDoubleFunction]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaLongToDoubleFunction[A0](sf: scala.Function1[A0, Double])(implicit evA0: =:=[A0, Long]): RichFunction1AsLongToDoubleFunction = new RichFunction1AsLongToDoubleFunction(sf.asInstanceOf[scala.Function1[Long, Double]])
  
  /** Enriches a Scala `Function1` with an `asJava` method that converts it to a `java.util.function.LongToIntFunction`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Long`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Long`
   *  @return a [[FunctionWrappers.RichFunction1AsLongToIntFunction]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaLongToIntFunction[A0](sf: scala.Function1[A0, Int])(implicit evA0: =:=[A0, Long]): RichFunction1AsLongToIntFunction = new RichFunction1AsLongToIntFunction(sf.asInstanceOf[scala.Function1[Long, Int]])
  
  /** Enriches a Scala `Function1` with an `asJava` method that converts it to a `java.util.function.LongUnaryOperator`.
   *
   *  @tparam A0 the argument type of `sf`, constrained by `evA0` to be `Long`
   *  @param sf the Scala function to convert
   *  @param evA0 evidence that `A0` is `Long`
   *  @return a [[FunctionWrappers.RichFunction1AsLongUnaryOperator]] value class wrapping `sf`
   */
  @inline implicit def enrichAsJavaLongUnaryOperator[A0](sf: scala.Function1[A0, Long])(implicit evA0: =:=[A0, Long]): RichFunction1AsLongUnaryOperator = new RichFunction1AsLongUnaryOperator(sf.asInstanceOf[scala.Function1[Long, Long]])
  
  
  
  /** Enriches a `java.util.function.BiConsumer` with an `asScala` method that converts it to a Scala `Function2`.
   *
   *  @tparam T the type of the first argument of `jf`
   *  @tparam U the type of the second argument of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichBiConsumerAsFunction2]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromBiConsumer[T, U](jf: java.util.function.BiConsumer[T, U]): RichBiConsumerAsFunction2[T, U] = new RichBiConsumerAsFunction2[T, U](jf)
  
  /** Enriches a `java.util.function.BiFunction` with an `asScala` method that converts it to a Scala `Function2`.
   *
   *  @tparam T the type of the first argument of `jf`
   *  @tparam U the type of the second argument of `jf`
   *  @tparam R the result type of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichBiFunctionAsFunction2]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromBiFunction[T, U, R](jf: java.util.function.BiFunction[T, U, R]): RichBiFunctionAsFunction2[T, U, R] = new RichBiFunctionAsFunction2[T, U, R](jf)
  
  /** Enriches a `java.util.function.BiPredicate` with an `asScala` method that converts it to a Scala `Function2`.
   *
   *  @tparam T the type of the first argument of `jf`
   *  @tparam U the type of the second argument of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichBiPredicateAsFunction2]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromBiPredicate[T, U](jf: java.util.function.BiPredicate[T, U]): RichBiPredicateAsFunction2[T, U] = new RichBiPredicateAsFunction2[T, U](jf)
  
  /** Enriches a `java.util.function.BinaryOperator` with an `asScala` method that converts it to a Scala `Function2`.
   *
   *  @tparam T the operand and result type of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichBinaryOperatorAsFunction2]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromBinaryOperator[T](jf: java.util.function.BinaryOperator[T]): RichBinaryOperatorAsFunction2[T] = new RichBinaryOperatorAsFunction2[T](jf)
  
  /** Enriches a `java.util.function.BooleanSupplier` with an `asScala` method that converts it to a Scala `Function0`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichBooleanSupplierAsFunction0]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromBooleanSupplier(jf: java.util.function.BooleanSupplier): RichBooleanSupplierAsFunction0 = new RichBooleanSupplierAsFunction0(jf)
  
  /** Enriches a `java.util.function.Consumer` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @tparam T the argument type of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichConsumerAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromConsumer[T](jf: java.util.function.Consumer[T]): RichConsumerAsFunction1[T] = new RichConsumerAsFunction1[T](jf)
  
  /** Enriches a `java.util.function.DoubleBinaryOperator` with an `asScala` method that converts it to a Scala `Function2`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichDoubleBinaryOperatorAsFunction2]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromDoubleBinaryOperator(jf: java.util.function.DoubleBinaryOperator): RichDoubleBinaryOperatorAsFunction2 = new RichDoubleBinaryOperatorAsFunction2(jf)
  
  /** Enriches a `java.util.function.DoubleConsumer` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichDoubleConsumerAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromDoubleConsumer(jf: java.util.function.DoubleConsumer): RichDoubleConsumerAsFunction1 = new RichDoubleConsumerAsFunction1(jf)
  
  /** Enriches a `java.util.function.DoubleFunction` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @tparam R the result type of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichDoubleFunctionAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromDoubleFunction[R](jf: java.util.function.DoubleFunction[R]): RichDoubleFunctionAsFunction1[R] = new RichDoubleFunctionAsFunction1[R](jf)
  
  /** Enriches a `java.util.function.DoublePredicate` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichDoublePredicateAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromDoublePredicate(jf: java.util.function.DoublePredicate): RichDoublePredicateAsFunction1 = new RichDoublePredicateAsFunction1(jf)
  
  /** Enriches a `java.util.function.DoubleSupplier` with an `asScala` method that converts it to a Scala `Function0`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichDoubleSupplierAsFunction0]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromDoubleSupplier(jf: java.util.function.DoubleSupplier): RichDoubleSupplierAsFunction0 = new RichDoubleSupplierAsFunction0(jf)
  
  /** Enriches a `java.util.function.DoubleToIntFunction` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichDoubleToIntFunctionAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromDoubleToIntFunction(jf: java.util.function.DoubleToIntFunction): RichDoubleToIntFunctionAsFunction1 = new RichDoubleToIntFunctionAsFunction1(jf)
  
  /** Enriches a `java.util.function.DoubleToLongFunction` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichDoubleToLongFunctionAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromDoubleToLongFunction(jf: java.util.function.DoubleToLongFunction): RichDoubleToLongFunctionAsFunction1 = new RichDoubleToLongFunctionAsFunction1(jf)
  
  /** Enriches a `java.util.function.DoubleUnaryOperator` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichDoubleUnaryOperatorAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromDoubleUnaryOperator(jf: java.util.function.DoubleUnaryOperator): RichDoubleUnaryOperatorAsFunction1 = new RichDoubleUnaryOperatorAsFunction1(jf)
  
  /** Enriches a `java.util.function.Function` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @tparam T the argument type of `jf`
   *  @tparam R the result type of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichFunctionAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromFunction[T, R](jf: java.util.function.Function[T, R]): RichFunctionAsFunction1[T, R] = new RichFunctionAsFunction1[T, R](jf)
  
  /** Enriches a `java.util.function.IntBinaryOperator` with an `asScala` method that converts it to a Scala `Function2`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichIntBinaryOperatorAsFunction2]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromIntBinaryOperator(jf: java.util.function.IntBinaryOperator): RichIntBinaryOperatorAsFunction2 = new RichIntBinaryOperatorAsFunction2(jf)
  
  /** Enriches a `java.util.function.IntConsumer` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichIntConsumerAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromIntConsumer(jf: java.util.function.IntConsumer): RichIntConsumerAsFunction1 = new RichIntConsumerAsFunction1(jf)
  
  /** Enriches a `java.util.function.IntFunction` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @tparam R the result type of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichIntFunctionAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromIntFunction[R](jf: java.util.function.IntFunction[R]): RichIntFunctionAsFunction1[R] = new RichIntFunctionAsFunction1[R](jf)
  
  /** Enriches a `java.util.function.IntPredicate` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichIntPredicateAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromIntPredicate(jf: java.util.function.IntPredicate): RichIntPredicateAsFunction1 = new RichIntPredicateAsFunction1(jf)
  
  /** Enriches a `java.util.function.IntSupplier` with an `asScala` method that converts it to a Scala `Function0`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichIntSupplierAsFunction0]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromIntSupplier(jf: java.util.function.IntSupplier): RichIntSupplierAsFunction0 = new RichIntSupplierAsFunction0(jf)
  
  /** Enriches a `java.util.function.IntToDoubleFunction` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichIntToDoubleFunctionAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromIntToDoubleFunction(jf: java.util.function.IntToDoubleFunction): RichIntToDoubleFunctionAsFunction1 = new RichIntToDoubleFunctionAsFunction1(jf)
  
  /** Enriches a `java.util.function.IntToLongFunction` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichIntToLongFunctionAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromIntToLongFunction(jf: java.util.function.IntToLongFunction): RichIntToLongFunctionAsFunction1 = new RichIntToLongFunctionAsFunction1(jf)
  
  /** Enriches a `java.util.function.IntUnaryOperator` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichIntUnaryOperatorAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromIntUnaryOperator(jf: java.util.function.IntUnaryOperator): RichIntUnaryOperatorAsFunction1 = new RichIntUnaryOperatorAsFunction1(jf)
  
  /** Enriches a `java.util.function.LongBinaryOperator` with an `asScala` method that converts it to a Scala `Function2`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichLongBinaryOperatorAsFunction2]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromLongBinaryOperator(jf: java.util.function.LongBinaryOperator): RichLongBinaryOperatorAsFunction2 = new RichLongBinaryOperatorAsFunction2(jf)
  
  /** Enriches a `java.util.function.LongConsumer` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichLongConsumerAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromLongConsumer(jf: java.util.function.LongConsumer): RichLongConsumerAsFunction1 = new RichLongConsumerAsFunction1(jf)
  
  /** Enriches a `java.util.function.LongFunction` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @tparam R the result type of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichLongFunctionAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromLongFunction[R](jf: java.util.function.LongFunction[R]): RichLongFunctionAsFunction1[R] = new RichLongFunctionAsFunction1[R](jf)
  
  /** Enriches a `java.util.function.LongPredicate` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichLongPredicateAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromLongPredicate(jf: java.util.function.LongPredicate): RichLongPredicateAsFunction1 = new RichLongPredicateAsFunction1(jf)
  
  /** Enriches a `java.util.function.LongSupplier` with an `asScala` method that converts it to a Scala `Function0`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichLongSupplierAsFunction0]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromLongSupplier(jf: java.util.function.LongSupplier): RichLongSupplierAsFunction0 = new RichLongSupplierAsFunction0(jf)
  
  /** Enriches a `java.util.function.LongToDoubleFunction` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichLongToDoubleFunctionAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromLongToDoubleFunction(jf: java.util.function.LongToDoubleFunction): RichLongToDoubleFunctionAsFunction1 = new RichLongToDoubleFunctionAsFunction1(jf)
  
  /** Enriches a `java.util.function.LongToIntFunction` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichLongToIntFunctionAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromLongToIntFunction(jf: java.util.function.LongToIntFunction): RichLongToIntFunctionAsFunction1 = new RichLongToIntFunctionAsFunction1(jf)
  
  /** Enriches a `java.util.function.LongUnaryOperator` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichLongUnaryOperatorAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromLongUnaryOperator(jf: java.util.function.LongUnaryOperator): RichLongUnaryOperatorAsFunction1 = new RichLongUnaryOperatorAsFunction1(jf)
  
  /** Enriches a `java.util.function.ObjDoubleConsumer` with an `asScala` method that converts it to a Scala `Function2`.
   *
   *  @tparam T the type of the first argument of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichObjDoubleConsumerAsFunction2]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromObjDoubleConsumer[T](jf: java.util.function.ObjDoubleConsumer[T]): RichObjDoubleConsumerAsFunction2[T] = new RichObjDoubleConsumerAsFunction2[T](jf)
  
  /** Enriches a `java.util.function.ObjIntConsumer` with an `asScala` method that converts it to a Scala `Function2`.
   *
   *  @tparam T the type of the first argument of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichObjIntConsumerAsFunction2]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromObjIntConsumer[T](jf: java.util.function.ObjIntConsumer[T]): RichObjIntConsumerAsFunction2[T] = new RichObjIntConsumerAsFunction2[T](jf)
  
  /** Enriches a `java.util.function.ObjLongConsumer` with an `asScala` method that converts it to a Scala `Function2`.
   *
   *  @tparam T the type of the first argument of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichObjLongConsumerAsFunction2]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromObjLongConsumer[T](jf: java.util.function.ObjLongConsumer[T]): RichObjLongConsumerAsFunction2[T] = new RichObjLongConsumerAsFunction2[T](jf)
  
  /** Enriches a `java.util.function.Predicate` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @tparam T the argument type of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichPredicateAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromPredicate[T](jf: java.util.function.Predicate[T]): RichPredicateAsFunction1[T] = new RichPredicateAsFunction1[T](jf)
  
  /** Enriches a `java.util.function.Supplier` with an `asScala` method that converts it to a Scala `Function0`.
   *
   *  @tparam T the result type of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichSupplierAsFunction0]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromSupplier[T](jf: java.util.function.Supplier[T]): RichSupplierAsFunction0[T] = new RichSupplierAsFunction0[T](jf)
  
  /** Enriches a `java.util.function.ToDoubleBiFunction` with an `asScala` method that converts it to a Scala `Function2`.
   *
   *  @tparam T the type of the first argument of `jf`
   *  @tparam U the type of the second argument of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichToDoubleBiFunctionAsFunction2]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromToDoubleBiFunction[T, U](jf: java.util.function.ToDoubleBiFunction[T, U]): RichToDoubleBiFunctionAsFunction2[T, U] = new RichToDoubleBiFunctionAsFunction2[T, U](jf)
  
  /** Enriches a `java.util.function.ToDoubleFunction` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @tparam T the argument type of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichToDoubleFunctionAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromToDoubleFunction[T](jf: java.util.function.ToDoubleFunction[T]): RichToDoubleFunctionAsFunction1[T] = new RichToDoubleFunctionAsFunction1[T](jf)
  
  /** Enriches a `java.util.function.ToIntBiFunction` with an `asScala` method that converts it to a Scala `Function2`.
   *
   *  @tparam T the type of the first argument of `jf`
   *  @tparam U the type of the second argument of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichToIntBiFunctionAsFunction2]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromToIntBiFunction[T, U](jf: java.util.function.ToIntBiFunction[T, U]): RichToIntBiFunctionAsFunction2[T, U] = new RichToIntBiFunctionAsFunction2[T, U](jf)
  
  /** Enriches a `java.util.function.ToIntFunction` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @tparam T the argument type of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichToIntFunctionAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromToIntFunction[T](jf: java.util.function.ToIntFunction[T]): RichToIntFunctionAsFunction1[T] = new RichToIntFunctionAsFunction1[T](jf)
  
  /** Enriches a `java.util.function.ToLongBiFunction` with an `asScala` method that converts it to a Scala `Function2`.
   *
   *  @tparam T the type of the first argument of `jf`
   *  @tparam U the type of the second argument of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichToLongBiFunctionAsFunction2]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromToLongBiFunction[T, U](jf: java.util.function.ToLongBiFunction[T, U]): RichToLongBiFunctionAsFunction2[T, U] = new RichToLongBiFunctionAsFunction2[T, U](jf)
  
  /** Enriches a `java.util.function.ToLongFunction` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @tparam T the argument type of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichToLongFunctionAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromToLongFunction[T](jf: java.util.function.ToLongFunction[T]): RichToLongFunctionAsFunction1[T] = new RichToLongFunctionAsFunction1[T](jf)
  
  /** Enriches a `java.util.function.UnaryOperator` with an `asScala` method that converts it to a Scala `Function1`.
   *
   *  @tparam T the operand and result type of `jf`
   *  @param jf the Java function to convert
   *  @return a [[FunctionWrappers.RichUnaryOperatorAsFunction1]] value class wrapping `jf`
   */
  @inline implicit def enrichAsScalaFromUnaryOperator[T](jf: java.util.function.UnaryOperator[T]): RichUnaryOperatorAsFunction1[T] = new RichUnaryOperatorAsFunction1[T](jf)
}
