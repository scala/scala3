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

object FunctionWrappers {
  /** A Scala `Function2` that delegates to a Java `BiConsumer`.
   *
   *  @tparam T the first input type of the bi-consumer
   *  @tparam U the second input type of the bi-consumer
   *  @param jf the Java `BiConsumer` to which `apply` delegates
   */
  case class FromJavaBiConsumer[T, U](jf: java.util.function.BiConsumer[T, U]) extends scala.Function2[T, U, Unit] {
    /** Invokes the wrapped Java `BiConsumer` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `jf`
     *  @param x2 the second argument to pass to `jf`
     *  @return `()`, since the delegate is called only for its side effect
     */
    def apply(x1: T, x2: U) = jf.accept(x1, x2)
  }
  
  /** A value class that adds an `asScala` method to a Java `BiConsumer`, converting it to a Scala `Function2`.
   *
   *  @tparam T the first input type of the bi-consumer
   *  @tparam U the second input type of the bi-consumer
   *  @param underlying the Java `BiConsumer` to convert
   */
  class RichBiConsumerAsFunction2[T, U](private val underlying: java.util.function.BiConsumer[T, U]) extends AnyVal {
    /** Returns a Scala `Function2` that calls `underlying`, or, if `underlying` is an `AsJavaBiConsumer`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function2[T, U, Unit] = underlying match {
      case AsJavaBiConsumer((sf @ _)) => sf.asInstanceOf[scala.Function2[T, U, Unit]]
      case _ => new FromJavaBiConsumer[T, U](underlying)
    }
  }
  
  /** A Java `BiConsumer` that delegates to a Scala `Function2`.
   *
   *  @tparam T the first input type of the bi-consumer
   *  @tparam U the second input type of the bi-consumer
   *  @param sf the Scala `Function2` to which `accept` delegates
   */
  case class AsJavaBiConsumer[T, U](sf: scala.Function2[T, U, Unit]) extends java.util.function.BiConsumer[T, U] {
    /** Invokes the wrapped Scala `Function2` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `sf`
     *  @param x2 the second argument to pass to `sf`
     *  @return `()`, since the delegate is called only for its side effect
     */
    def accept(x1: T, x2: U) = sf.apply(x1, x2)
  }
  
  /** A value class that adds `asJava` and `asJavaBiConsumer` methods to a Scala `Function2`, converting it to a Java `BiConsumer`.
   *
   *  @tparam T the first input type of the bi-consumer
   *  @tparam U the second input type of the bi-consumer
   *  @param underlying the Scala `Function2` to convert
   */
  class RichFunction2AsBiConsumer[T, U](private val underlying: scala.Function2[T, U, Unit]) extends AnyVal {
    /** Returns a Java `BiConsumer` that calls `underlying`, or, if `underlying` is a `FromJavaBiConsumer`, the Java `BiConsumer` that wrapper holds. */
    @inline def asJava: java.util.function.BiConsumer[T, U] = underlying match {
      case FromJavaBiConsumer((jf @ _)) => jf.asInstanceOf[java.util.function.BiConsumer[T, U]]
      case _ => new AsJavaBiConsumer[T, U](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `BiConsumer` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaBiConsumer: java.util.function.BiConsumer[T, U] = underlying match {
      case FromJavaBiConsumer((sf @ _)) => sf.asInstanceOf[java.util.function.BiConsumer[T, U]]
      case _ => new AsJavaBiConsumer[T, U](underlying)
    }
  }
  
  
  /** A Scala `Function2` that delegates to a Java `BiFunction`.
   *
   *  @tparam T the first input type of the bi-function
   *  @tparam U the second input type of the bi-function
   *  @tparam R the return type of the bi-function
   *  @param jf the Java `BiFunction` to which `apply` delegates
   */
  case class FromJavaBiFunction[T, U, R](jf: java.util.function.BiFunction[T, U, R]) extends scala.Function2[T, U, R] {
    /** Returns the result of invoking the wrapped Java `BiFunction` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `jf`
     *  @param x2 the second argument to pass to `jf`
     */
    def apply(x1: T, x2: U) = jf.apply(x1, x2)
  }
  
  /** A value class that adds an `asScala` method to a Java `BiFunction`, converting it to a Scala `Function2`.
   *
   *  @tparam T the first input type of the bi-function
   *  @tparam U the second input type of the bi-function
   *  @tparam R the return type of the bi-function
   *  @param underlying the Java `BiFunction` to convert
   */
  class RichBiFunctionAsFunction2[T, U, R](private val underlying: java.util.function.BiFunction[T, U, R]) extends AnyVal {
    /** Returns a Scala `Function2` that calls `underlying`, or, if `underlying` is an `AsJavaBiFunction`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function2[T, U, R] = underlying match {
      case AsJavaBiFunction((sf @ _)) => sf.asInstanceOf[scala.Function2[T, U, R]]
      case _ => new FromJavaBiFunction[T, U, R](underlying)
    }
  }
  
  /** A Java `BiFunction` that delegates to a Scala `Function2`.
   *
   *  @tparam T the first input type of the bi-function
   *  @tparam U the second input type of the bi-function
   *  @tparam R the return type of the bi-function
   *  @param sf the Scala `Function2` to which `apply` delegates
   */
  case class AsJavaBiFunction[T, U, R](sf: scala.Function2[T, U, R]) extends java.util.function.BiFunction[T, U, R] {
    /** Returns the result of invoking the wrapped Scala `Function2` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `sf`
     *  @param x2 the second argument to pass to `sf`
     */
    def apply(x1: T, x2: U): R = sf.apply(x1, x2)
  }
  
  /** A value class that adds `asJava` and `asJavaBiFunction` methods to a Scala `Function2`, converting it to a Java `BiFunction`.
   *
   *  @tparam T the first input type of the bi-function
   *  @tparam U the second input type of the bi-function
   *  @tparam R the return type of the bi-function
   *  @param underlying the Scala `Function2` to convert
   */
  class RichFunction2AsBiFunction[T, U, R](private val underlying: scala.Function2[T, U, R]) extends AnyVal {
    /** Returns a Java `BiFunction` that calls `underlying`, or, if `underlying` is a `FromJavaBiFunction`, the Java `BiFunction` that wrapper holds. */
    @inline def asJava: java.util.function.BiFunction[T, U, R] = underlying match {
      case FromJavaBiFunction((jf @ _)) => jf.asInstanceOf[java.util.function.BiFunction[T, U, R]]
      case _ => new AsJavaBiFunction[T, U, R](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `BiFunction` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaBiFunction: java.util.function.BiFunction[T, U, R] = underlying match {
      case FromJavaBiFunction((sf @ _)) => sf.asInstanceOf[java.util.function.BiFunction[T, U, R]]
      case _ => new AsJavaBiFunction[T, U, R](underlying)
    }
  }
  
  
  /** A Scala `Function2` that delegates to a Java `BiPredicate`.
   *
   *  @tparam T the first input type of the bi-predicate
   *  @tparam U the second input type of the bi-predicate
   *  @param jf the Java `BiPredicate` to which `apply` delegates
   */
  case class FromJavaBiPredicate[T, U](jf: java.util.function.BiPredicate[T, U]) extends scala.Function2[T, U, Boolean] {
    /** Returns the result of invoking the wrapped Java `BiPredicate` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `jf`
     *  @param x2 the second argument to pass to `jf`
     */
    def apply(x1: T, x2: U) = jf.test(x1, x2)
  }
  
  /** A value class that adds an `asScala` method to a Java `BiPredicate`, converting it to a Scala `Function2`.
   *
   *  @tparam T the first input type of the bi-predicate
   *  @tparam U the second input type of the bi-predicate
   *  @param underlying the Java `BiPredicate` to convert
   */
  class RichBiPredicateAsFunction2[T, U](private val underlying: java.util.function.BiPredicate[T, U]) extends AnyVal {
    /** Returns a Scala `Function2` that calls `underlying`, or, if `underlying` is an `AsJavaBiPredicate`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function2[T, U, Boolean] = underlying match {
      case AsJavaBiPredicate((sf @ _)) => sf.asInstanceOf[scala.Function2[T, U, Boolean]]
      case _ => new FromJavaBiPredicate[T, U](underlying)
    }
  }
  
  /** A Java `BiPredicate` that delegates to a Scala `Function2`.
   *
   *  @tparam T the first input type of the bi-predicate
   *  @tparam U the second input type of the bi-predicate
   *  @param sf the Scala `Function2` to which `test` delegates
   */
  case class AsJavaBiPredicate[T, U](sf: scala.Function2[T, U, Boolean]) extends java.util.function.BiPredicate[T, U] {
    /** Returns the result of invoking the wrapped Scala `Function2` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `sf`
     *  @param x2 the second argument to pass to `sf`
     */
    def test(x1: T, x2: U) = sf.apply(x1, x2)
  }
  
  /** A value class that adds `asJava` and `asJavaBiPredicate` methods to a Scala `Function2`, converting it to a Java `BiPredicate`.
   *
   *  @tparam T the first input type of the bi-predicate
   *  @tparam U the second input type of the bi-predicate
   *  @param underlying the Scala `Function2` to convert
   */
  class RichFunction2AsBiPredicate[T, U](private val underlying: scala.Function2[T, U, Boolean]) extends AnyVal {
    /** Returns a Java `BiPredicate` that calls `underlying`, or, if `underlying` is a `FromJavaBiPredicate`, the Java `BiPredicate` that wrapper holds. */
    @inline def asJava: java.util.function.BiPredicate[T, U] = underlying match {
      case FromJavaBiPredicate((jf @ _)) => jf.asInstanceOf[java.util.function.BiPredicate[T, U]]
      case _ => new AsJavaBiPredicate[T, U](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `BiPredicate` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaBiPredicate: java.util.function.BiPredicate[T, U] = underlying match {
      case FromJavaBiPredicate((sf @ _)) => sf.asInstanceOf[java.util.function.BiPredicate[T, U]]
      case _ => new AsJavaBiPredicate[T, U](underlying)
    }
  }
  
  
  /** A Scala `Function2` that delegates to a Java `BinaryOperator`.
   *
   *  @tparam T the input and output type of the binary operator
   *  @param jf the Java `BinaryOperator` to which `apply` delegates
   */
  case class FromJavaBinaryOperator[T](jf: java.util.function.BinaryOperator[T]) extends scala.Function2[T, T, T] {
    /** Returns the result of invoking the wrapped Java `BinaryOperator` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `jf`
     *  @param x2 the second argument to pass to `jf`
     */
    def apply(x1: T, x2: T) = jf.apply(x1, x2)
  }
  
  /** A value class that adds an `asScala` method to a Java `BinaryOperator`, converting it to a Scala `Function2`.
   *
   *  @tparam T the input and output type of the binary operator
   *  @param underlying the Java `BinaryOperator` to convert
   */
  class RichBinaryOperatorAsFunction2[T](private val underlying: java.util.function.BinaryOperator[T]) extends AnyVal {
    /** Returns a Scala `Function2` that calls `underlying`, or, if `underlying` is an `AsJavaBinaryOperator`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function2[T, T, T] = underlying match {
      case AsJavaBinaryOperator((sf @ _)) => sf.asInstanceOf[scala.Function2[T, T, T]]
      case _ => new FromJavaBinaryOperator[T](underlying)
    }
  }
  
  /** A Java `BinaryOperator` that delegates to a Scala `Function2`.
   *
   *  @tparam T the input and output type of the binary operator
   *  @param sf the Scala `Function2` to which `apply` delegates
   */
  case class AsJavaBinaryOperator[T](sf: scala.Function2[T, T, T]) extends java.util.function.BinaryOperator[T] {
    /** Returns the result of invoking the wrapped Scala `Function2` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `sf`
     *  @param x2 the second argument to pass to `sf`
     */
    def apply(x1: T, x2: T): T = sf.apply(x1, x2)
  }
  
  /** A value class that adds `asJava` and `asJavaBinaryOperator` methods to a Scala `Function2`, converting it to a Java `BinaryOperator`.
   *
   *  @tparam T the input and output type of the binary operator
   *  @param underlying the Scala `Function2` to convert
   */
  class RichFunction2AsBinaryOperator[T](private val underlying: scala.Function2[T, T, T]) extends AnyVal {
    /** Returns a Java `BinaryOperator` that calls `underlying`, or, if `underlying` is a `FromJavaBinaryOperator`, the Java `BinaryOperator` that wrapper holds. */
    @inline def asJava: java.util.function.BinaryOperator[T] = underlying match {
      case FromJavaBinaryOperator((jf @ _)) => jf.asInstanceOf[java.util.function.BinaryOperator[T]]
      case _ => new AsJavaBinaryOperator[T](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `BinaryOperator` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaBinaryOperator: java.util.function.BinaryOperator[T] = underlying match {
      case FromJavaBinaryOperator((sf @ _)) => sf.asInstanceOf[java.util.function.BinaryOperator[T]]
      case _ => new AsJavaBinaryOperator[T](underlying)
    }
  }
  
  
  /** A Scala `Function0` that delegates to a Java `BooleanSupplier`.
   *
   *  @param jf the Java `BooleanSupplier` to which `apply` delegates
   */
  case class FromJavaBooleanSupplier(jf: java.util.function.BooleanSupplier) extends scala.Function0[Boolean] {
    /** Returns the value produced by the wrapped Java `BooleanSupplier`. */
    def apply() = jf.getAsBoolean()
  }
  
  /** A value class that adds an `asScala` method to a Java `BooleanSupplier`, converting it to a Scala `Function0`.
   *
   *  @param underlying the Java `BooleanSupplier` to convert
   */
  class RichBooleanSupplierAsFunction0(private val underlying: java.util.function.BooleanSupplier) extends AnyVal {
    /** Returns a Scala `Function0` that calls `underlying`, or, if `underlying` is an `AsJavaBooleanSupplier`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function0[Boolean] = underlying match {
      case AsJavaBooleanSupplier((sf @ _)) => sf.asInstanceOf[scala.Function0[Boolean]]
      case _ => new FromJavaBooleanSupplier(underlying)
    }
  }
  
  /** A Java `BooleanSupplier` that delegates to a Scala `Function0`.
   *
   *  @param sf the Scala `Function0` to which `getAsBoolean` delegates
   */
  case class AsJavaBooleanSupplier(sf: scala.Function0[Boolean]) extends java.util.function.BooleanSupplier {
    /** Returns the value produced by the wrapped Scala `Function0`. */
    def getAsBoolean() = sf.apply()
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function0`, converting it to a Java `BooleanSupplier`.
   *
   *  @param underlying the Scala `Function0` to convert
   */
  class RichFunction0AsBooleanSupplier(private val underlying: scala.Function0[Boolean]) extends AnyVal {
    /** Returns a Java `BooleanSupplier` that calls `underlying`, or, if `underlying` is a `FromJavaBooleanSupplier`, the Java `BooleanSupplier` that wrapper holds. */
    @inline def asJava: java.util.function.BooleanSupplier = underlying match {
      case FromJavaBooleanSupplier((jf @ _)) => jf.asInstanceOf[java.util.function.BooleanSupplier]
      case _ => new AsJavaBooleanSupplier(underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `Consumer`.
   *
   *  @tparam T the input type of the consumer
   *  @param jf the Java `Consumer` to which `apply` delegates
   */
  case class FromJavaConsumer[T](jf: java.util.function.Consumer[T]) extends scala.Function1[T, Unit] {
    /** Invokes the wrapped Java `Consumer` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     *  @return `()`, since the delegate is called only for its side effect
     */
    def apply(x1: T) = jf.accept(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `Consumer`, converting it to a Scala `Function1`.
   *
   *  @tparam T the input type of the consumer
   *  @param underlying the Java `Consumer` to convert
   */
  class RichConsumerAsFunction1[T](private val underlying: java.util.function.Consumer[T]) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaConsumer`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[T, Unit] = underlying match {
      case AsJavaConsumer((sf @ _)) => sf.asInstanceOf[scala.Function1[T, Unit]]
      case _ => new FromJavaConsumer[T](underlying)
    }
  }
  
  /** A Java `Consumer` that delegates to a Scala `Function1`.
   *
   *  @tparam T the input type of the consumer
   *  @param sf the Scala `Function1` to which `accept` delegates
   */
  case class AsJavaConsumer[T](sf: scala.Function1[T, Unit]) extends java.util.function.Consumer[T] {
    /** Invokes the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     *  @return `()`, since the delegate is called only for its side effect
     */
    def accept(x1: T) = sf.apply(x1)
  }
  
  /** A value class that adds `asJava` and `asJavaConsumer` methods to a Scala `Function1`, converting it to a Java `Consumer`.
   *
   *  @tparam T the input type of the consumer
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsConsumer[T](private val underlying: scala.Function1[T, Unit]) extends AnyVal {
    /** Returns a Java `Consumer` that calls `underlying`, or, if `underlying` is a `FromJavaConsumer`, the Java `Consumer` that wrapper holds. */
    @inline def asJava: java.util.function.Consumer[T] = underlying match {
      case FromJavaConsumer((jf @ _)) => jf.asInstanceOf[java.util.function.Consumer[T]]
      case _ => new AsJavaConsumer[T](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `Consumer` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaConsumer: java.util.function.Consumer[T] = underlying match {
      case FromJavaConsumer((sf @ _)) => sf.asInstanceOf[java.util.function.Consumer[T]]
      case _ => new AsJavaConsumer[T](underlying)
    }
  }
  
  
  /** A Scala `Function2` that delegates to a Java `DoubleBinaryOperator`.
   *
   *  @param jf the Java `DoubleBinaryOperator` to which `apply` delegates
   */
  case class FromJavaDoubleBinaryOperator(jf: java.util.function.DoubleBinaryOperator) extends scala.Function2[Double, Double, Double] {
    /** Returns the result of invoking the wrapped Java `DoubleBinaryOperator` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `jf`
     *  @param x2 the second argument to pass to `jf`
     */
    def apply(x1: scala.Double, x2: scala.Double) = jf.applyAsDouble(x1, x2)
  }
  
  /** A value class that adds an `asScala` method to a Java `DoubleBinaryOperator`, converting it to a Scala `Function2`.
   *
   *  @param underlying the Java `DoubleBinaryOperator` to convert
   */
  class RichDoubleBinaryOperatorAsFunction2(private val underlying: java.util.function.DoubleBinaryOperator) extends AnyVal {
    /** Returns a Scala `Function2` that calls `underlying`, or, if `underlying` is an `AsJavaDoubleBinaryOperator`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function2[Double, Double, Double] = underlying match {
      case AsJavaDoubleBinaryOperator((sf @ _)) => sf.asInstanceOf[scala.Function2[Double, Double, Double]]
      case _ => new FromJavaDoubleBinaryOperator(underlying)
    }
  }
  
  /** A Java `DoubleBinaryOperator` that delegates to a Scala `Function2`.
   *
   *  @param sf the Scala `Function2` to which `applyAsDouble` delegates
   */
  case class AsJavaDoubleBinaryOperator(sf: scala.Function2[Double, Double, Double]) extends java.util.function.DoubleBinaryOperator {
    /** Returns the result of invoking the wrapped Scala `Function2` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `sf`
     *  @param x2 the second argument to pass to `sf`
     */
    def applyAsDouble(x1: scala.Double, x2: scala.Double) = sf.apply(x1, x2)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function2`, converting it to a Java `DoubleBinaryOperator`.
   *
   *  @param underlying the Scala `Function2` to convert
   */
  class RichFunction2AsDoubleBinaryOperator(private val underlying: scala.Function2[Double, Double, Double]) extends AnyVal {
    /** Returns a Java `DoubleBinaryOperator` that calls `underlying`, or, if `underlying` is a `FromJavaDoubleBinaryOperator`, the Java `DoubleBinaryOperator` that wrapper holds. */
    @inline def asJava: java.util.function.DoubleBinaryOperator = underlying match {
      case FromJavaDoubleBinaryOperator((jf @ _)) => jf.asInstanceOf[java.util.function.DoubleBinaryOperator]
      case _ => new AsJavaDoubleBinaryOperator(underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `DoubleConsumer`.
   *
   *  @param jf the Java `DoubleConsumer` to which `apply` delegates
   */
  case class FromJavaDoubleConsumer(jf: java.util.function.DoubleConsumer) extends scala.Function1[Double, Unit] {
    /** Invokes the wrapped Java `DoubleConsumer` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     *  @return `()`, since the delegate is called only for its side effect
     */
    def apply(x1: scala.Double) = jf.accept(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `DoubleConsumer`, converting it to a Scala `Function1`.
   *
   *  @param underlying the Java `DoubleConsumer` to convert
   */
  class RichDoubleConsumerAsFunction1(private val underlying: java.util.function.DoubleConsumer) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaDoubleConsumer`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Double, Unit] = underlying match {
      case AsJavaDoubleConsumer((sf @ _)) => sf.asInstanceOf[scala.Function1[Double, Unit]]
      case _ => new FromJavaDoubleConsumer(underlying)
    }
  }
  
  /** A Java `DoubleConsumer` that delegates to a Scala `Function1`.
   *
   *  @param sf the Scala `Function1` to which `accept` delegates
   */
  case class AsJavaDoubleConsumer(sf: scala.Function1[Double, Unit]) extends java.util.function.DoubleConsumer {
    /** Invokes the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     *  @return `()`, since the delegate is called only for its side effect
     */
    def accept(x1: scala.Double) = sf.apply(x1)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function1`, converting it to a Java `DoubleConsumer`.
   *
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsDoubleConsumer(private val underlying: scala.Function1[Double, Unit]) extends AnyVal {
    /** Returns a Java `DoubleConsumer` that calls `underlying`, or, if `underlying` is a `FromJavaDoubleConsumer`, the Java `DoubleConsumer` that wrapper holds. */
    @inline def asJava: java.util.function.DoubleConsumer = underlying match {
      case FromJavaDoubleConsumer((jf @ _)) => jf.asInstanceOf[java.util.function.DoubleConsumer]
      case _ => new AsJavaDoubleConsumer(underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `DoubleFunction`.
   *
   *  @tparam R the return type of the function
   *  @param jf the Java `DoubleFunction` to which `apply` delegates
   */
  case class FromJavaDoubleFunction[R](jf: java.util.function.DoubleFunction[R]) extends scala.Function1[Double, R] {
    /** Returns the result of invoking the wrapped Java `DoubleFunction` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: scala.Double) = jf.apply(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `DoubleFunction`, converting it to a Scala `Function1`.
   *
   *  @tparam R the return type of the function
   *  @param underlying the Java `DoubleFunction` to convert
   */
  class RichDoubleFunctionAsFunction1[R](private val underlying: java.util.function.DoubleFunction[R]) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaDoubleFunction`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Double, R] = underlying match {
      case AsJavaDoubleFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Double, R]]
      case _ => new FromJavaDoubleFunction[R](underlying)
    }
  }
  
  /** A Java `DoubleFunction` that delegates to a Scala `Function1`.
   *
   *  @tparam R the return type of the function
   *  @param sf the Scala `Function1` to which `apply` delegates
   */
  case class AsJavaDoubleFunction[R](sf: scala.Function1[Double, R]) extends java.util.function.DoubleFunction[R] {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def apply(x1: scala.Double): R = sf.apply(x1)
  }
  
  /** A value class that adds `asJava` and `asJavaDoubleFunction` methods to a Scala `Function1`, converting it to a Java `DoubleFunction`.
   *
   *  @tparam R the return type of the function
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsDoubleFunction[R](private val underlying: scala.Function1[Double, R]) extends AnyVal {
    /** Returns a Java `DoubleFunction` that calls `underlying`, or, if `underlying` is a `FromJavaDoubleFunction`, the Java `DoubleFunction` that wrapper holds. */
    @inline def asJava: java.util.function.DoubleFunction[R] = underlying match {
      case FromJavaDoubleFunction((jf @ _)) => jf.asInstanceOf[java.util.function.DoubleFunction[R]]
      case _ => new AsJavaDoubleFunction[R](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `DoubleFunction` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaDoubleFunction: java.util.function.DoubleFunction[R] = underlying match {
      case FromJavaDoubleFunction((sf @ _)) => sf.asInstanceOf[java.util.function.DoubleFunction[R]]
      case _ => new AsJavaDoubleFunction[R](underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `DoublePredicate`.
   *
   *  @param jf the Java `DoublePredicate` to which `apply` delegates
   */
  case class FromJavaDoublePredicate(jf: java.util.function.DoublePredicate) extends scala.Function1[Double, Boolean] {
    /** Returns the result of invoking the wrapped Java `DoublePredicate` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: scala.Double) = jf.test(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `DoublePredicate`, converting it to a Scala `Function1`.
   *
   *  @param underlying the Java `DoublePredicate` to convert
   */
  class RichDoublePredicateAsFunction1(private val underlying: java.util.function.DoublePredicate) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaDoublePredicate`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Double, Boolean] = underlying match {
      case AsJavaDoublePredicate((sf @ _)) => sf.asInstanceOf[scala.Function1[Double, Boolean]]
      case _ => new FromJavaDoublePredicate(underlying)
    }
  }
  
  /** A Java `DoublePredicate` that delegates to a Scala `Function1`.
   *
   *  @param sf the Scala `Function1` to which `test` delegates
   */
  case class AsJavaDoublePredicate(sf: scala.Function1[Double, Boolean]) extends java.util.function.DoublePredicate {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def test(x1: scala.Double) = sf.apply(x1)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function1`, converting it to a Java `DoublePredicate`.
   *
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsDoublePredicate(private val underlying: scala.Function1[Double, Boolean]) extends AnyVal {
    /** Returns a Java `DoublePredicate` that calls `underlying`, or, if `underlying` is a `FromJavaDoublePredicate`, the Java `DoublePredicate` that wrapper holds. */
    @inline def asJava: java.util.function.DoublePredicate = underlying match {
      case FromJavaDoublePredicate((jf @ _)) => jf.asInstanceOf[java.util.function.DoublePredicate]
      case _ => new AsJavaDoublePredicate(underlying)
    }
  }
  
  
  /** A Scala `Function0` that delegates to a Java `DoubleSupplier`.
   *
   *  @param jf the Java `DoubleSupplier` to which `apply` delegates
   */
  case class FromJavaDoubleSupplier(jf: java.util.function.DoubleSupplier) extends scala.Function0[Double] {
    /** Returns the value produced by the wrapped Java `DoubleSupplier`. */
    def apply() = jf.getAsDouble()
  }
  
  /** A value class that adds an `asScala` method to a Java `DoubleSupplier`, converting it to a Scala `Function0`.
   *
   *  @param underlying the Java `DoubleSupplier` to convert
   */
  class RichDoubleSupplierAsFunction0(private val underlying: java.util.function.DoubleSupplier) extends AnyVal {
    /** Returns a Scala `Function0` that calls `underlying`, or, if `underlying` is an `AsJavaDoubleSupplier`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function0[Double] = underlying match {
      case AsJavaDoubleSupplier((sf @ _)) => sf.asInstanceOf[scala.Function0[Double]]
      case _ => new FromJavaDoubleSupplier(underlying)
    }
  }
  
  /** A Java `DoubleSupplier` that delegates to a Scala `Function0`.
   *
   *  @param sf the Scala `Function0` to which `getAsDouble` delegates
   */
  case class AsJavaDoubleSupplier(sf: scala.Function0[Double]) extends java.util.function.DoubleSupplier {
    /** Returns the value produced by the wrapped Scala `Function0`. */
    def getAsDouble() = sf.apply()
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function0`, converting it to a Java `DoubleSupplier`.
   *
   *  @param underlying the Scala `Function0` to convert
   */
  class RichFunction0AsDoubleSupplier(private val underlying: scala.Function0[Double]) extends AnyVal {
    /** Returns a Java `DoubleSupplier` that calls `underlying`, or, if `underlying` is a `FromJavaDoubleSupplier`, the Java `DoubleSupplier` that wrapper holds. */
    @inline def asJava: java.util.function.DoubleSupplier = underlying match {
      case FromJavaDoubleSupplier((jf @ _)) => jf.asInstanceOf[java.util.function.DoubleSupplier]
      case _ => new AsJavaDoubleSupplier(underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `DoubleToIntFunction`.
   *
   *  @param jf the Java `DoubleToIntFunction` to which `apply` delegates
   */
  case class FromJavaDoubleToIntFunction(jf: java.util.function.DoubleToIntFunction) extends scala.Function1[Double, Int] {
    /** Returns the result of invoking the wrapped Java `DoubleToIntFunction` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: scala.Double) = jf.applyAsInt(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `DoubleToIntFunction`, converting it to a Scala `Function1`.
   *
   *  @param underlying the Java `DoubleToIntFunction` to convert
   */
  class RichDoubleToIntFunctionAsFunction1(private val underlying: java.util.function.DoubleToIntFunction) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaDoubleToIntFunction`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Double, Int] = underlying match {
      case AsJavaDoubleToIntFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Double, Int]]
      case _ => new FromJavaDoubleToIntFunction(underlying)
    }
  }
  
  /** A Java `DoubleToIntFunction` that delegates to a Scala `Function1`.
   *
   *  @param sf the Scala `Function1` to which `applyAsInt` delegates
   */
  case class AsJavaDoubleToIntFunction(sf: scala.Function1[Double, Int]) extends java.util.function.DoubleToIntFunction {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def applyAsInt(x1: scala.Double) = sf.apply(x1)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function1`, converting it to a Java `DoubleToIntFunction`.
   *
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsDoubleToIntFunction(private val underlying: scala.Function1[Double, Int]) extends AnyVal {
    /** Returns a Java `DoubleToIntFunction` that calls `underlying`, or, if `underlying` is a `FromJavaDoubleToIntFunction`, the Java `DoubleToIntFunction` that wrapper holds. */
    @inline def asJava: java.util.function.DoubleToIntFunction = underlying match {
      case FromJavaDoubleToIntFunction((jf @ _)) => jf.asInstanceOf[java.util.function.DoubleToIntFunction]
      case _ => new AsJavaDoubleToIntFunction(underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `DoubleToLongFunction`.
   *
   *  @param jf the Java `DoubleToLongFunction` to which `apply` delegates
   */
  case class FromJavaDoubleToLongFunction(jf: java.util.function.DoubleToLongFunction) extends scala.Function1[Double, Long] {
    /** Returns the result of invoking the wrapped Java `DoubleToLongFunction` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: scala.Double) = jf.applyAsLong(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `DoubleToLongFunction`, converting it to a Scala `Function1`.
   *
   *  @param underlying the Java `DoubleToLongFunction` to convert
   */
  class RichDoubleToLongFunctionAsFunction1(private val underlying: java.util.function.DoubleToLongFunction) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaDoubleToLongFunction`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Double, Long] = underlying match {
      case AsJavaDoubleToLongFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Double, Long]]
      case _ => new FromJavaDoubleToLongFunction(underlying)
    }
  }
  
  /** A Java `DoubleToLongFunction` that delegates to a Scala `Function1`.
   *
   *  @param sf the Scala `Function1` to which `applyAsLong` delegates
   */
  case class AsJavaDoubleToLongFunction(sf: scala.Function1[Double, Long]) extends java.util.function.DoubleToLongFunction {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def applyAsLong(x1: scala.Double) = sf.apply(x1)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function1`, converting it to a Java `DoubleToLongFunction`.
   *
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsDoubleToLongFunction(private val underlying: scala.Function1[Double, Long]) extends AnyVal {
    /** Returns a Java `DoubleToLongFunction` that calls `underlying`, or, if `underlying` is a `FromJavaDoubleToLongFunction`, the Java `DoubleToLongFunction` that wrapper holds. */
    @inline def asJava: java.util.function.DoubleToLongFunction = underlying match {
      case FromJavaDoubleToLongFunction((jf @ _)) => jf.asInstanceOf[java.util.function.DoubleToLongFunction]
      case _ => new AsJavaDoubleToLongFunction(underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `DoubleUnaryOperator`.
   *
   *  @param jf the Java `DoubleUnaryOperator` to which `apply` delegates
   */
  case class FromJavaDoubleUnaryOperator(jf: java.util.function.DoubleUnaryOperator) extends scala.Function1[Double, Double] {
    /** Returns the result of invoking the wrapped Java `DoubleUnaryOperator` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: scala.Double) = jf.applyAsDouble(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `DoubleUnaryOperator`, converting it to a Scala `Function1`.
   *
   *  @param underlying the Java `DoubleUnaryOperator` to convert
   */
  class RichDoubleUnaryOperatorAsFunction1(private val underlying: java.util.function.DoubleUnaryOperator) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaDoubleUnaryOperator`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Double, Double] = underlying match {
      case AsJavaDoubleUnaryOperator((sf @ _)) => sf.asInstanceOf[scala.Function1[Double, Double]]
      case _ => new FromJavaDoubleUnaryOperator(underlying)
    }
  }
  
  /** A Java `DoubleUnaryOperator` that delegates to a Scala `Function1`.
   *
   *  @param sf the Scala `Function1` to which `applyAsDouble` delegates
   */
  case class AsJavaDoubleUnaryOperator(sf: scala.Function1[Double, Double]) extends java.util.function.DoubleUnaryOperator {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def applyAsDouble(x1: scala.Double) = sf.apply(x1)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function1`, converting it to a Java `DoubleUnaryOperator`.
   *
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsDoubleUnaryOperator(private val underlying: scala.Function1[Double, Double]) extends AnyVal {
    /** Returns a Java `DoubleUnaryOperator` that calls `underlying`, or, if `underlying` is a `FromJavaDoubleUnaryOperator`, the Java `DoubleUnaryOperator` that wrapper holds. */
    @inline def asJava: java.util.function.DoubleUnaryOperator = underlying match {
      case FromJavaDoubleUnaryOperator((jf @ _)) => jf.asInstanceOf[java.util.function.DoubleUnaryOperator]
      case _ => new AsJavaDoubleUnaryOperator(underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `Function`.
   *
   *  @tparam T the input type of the function
   *  @tparam R the return type of the function
   *  @param jf the Java `Function` to which `apply` delegates
   */
  case class FromJavaFunction[T, R](jf: java.util.function.Function[T, R]) extends scala.Function1[T, R] {
    /** Returns the result of invoking the wrapped Java `Function` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: T) = jf.apply(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `Function`, converting it to a Scala `Function1`.
   *
   *  @tparam T the input type of the function
   *  @tparam R the return type of the function
   *  @param underlying the Java `Function` to convert
   */
  class RichFunctionAsFunction1[T, R](private val underlying: java.util.function.Function[T, R]) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaFunction`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[T, R] = underlying match {
      case AsJavaFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[T, R]]
      case _ => new FromJavaFunction[T, R](underlying)
    }
  }
  
  /** A Java `Function` that delegates to a Scala `Function1`.
   *
   *  @tparam T the input type of the function
   *  @tparam R the return type of the function
   *  @param sf the Scala `Function1` to which `apply` delegates
   */
  case class AsJavaFunction[T, R](sf: scala.Function1[T, R]) extends java.util.function.Function[T, R] {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def apply(x1: T): R = sf.apply(x1)
  }
  
  /** A value class that adds `asJava` and `asJavaFunction` methods to a Scala `Function1`, converting it to a Java `Function`.
   *
   *  @tparam T the input type of the function
   *  @tparam R the return type of the function
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsFunction[T, R](private val underlying: scala.Function1[T, R]) extends AnyVal {
    /** Returns a Java `Function` that calls `underlying`, or, if `underlying` is a `FromJavaFunction`, the Java `Function` that wrapper holds. */
    @inline def asJava: java.util.function.Function[T, R] = underlying match {
      case FromJavaFunction((jf @ _)) => jf.asInstanceOf[java.util.function.Function[T, R]]
      case _ => new AsJavaFunction[T, R](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `Function` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaFunction: java.util.function.Function[T, R] = underlying match {
      case FromJavaFunction((sf @ _)) => sf.asInstanceOf[java.util.function.Function[T, R]]
      case _ => new AsJavaFunction[T, R](underlying)
    }
  }
  
  
  /** A Scala `Function2` that delegates to a Java `IntBinaryOperator`.
   *
   *  @param jf the Java `IntBinaryOperator` to which `apply` delegates
   */
  case class FromJavaIntBinaryOperator(jf: java.util.function.IntBinaryOperator) extends scala.Function2[Int, Int, Int] {
    /** Returns the result of invoking the wrapped Java `IntBinaryOperator` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `jf`
     *  @param x2 the second argument to pass to `jf`
     */
    def apply(x1: scala.Int, x2: scala.Int) = jf.applyAsInt(x1, x2)
  }
  
  /** A value class that adds an `asScala` method to a Java `IntBinaryOperator`, converting it to a Scala `Function2`.
   *
   *  @param underlying the Java `IntBinaryOperator` to convert
   */
  class RichIntBinaryOperatorAsFunction2(private val underlying: java.util.function.IntBinaryOperator) extends AnyVal {
    /** Returns a Scala `Function2` that calls `underlying`, or, if `underlying` is an `AsJavaIntBinaryOperator`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function2[Int, Int, Int] = underlying match {
      case AsJavaIntBinaryOperator((sf @ _)) => sf.asInstanceOf[scala.Function2[Int, Int, Int]]
      case _ => new FromJavaIntBinaryOperator(underlying)
    }
  }
  
  /** A Java `IntBinaryOperator` that delegates to a Scala `Function2`.
   *
   *  @param sf the Scala `Function2` to which `applyAsInt` delegates
   */
  case class AsJavaIntBinaryOperator(sf: scala.Function2[Int, Int, Int]) extends java.util.function.IntBinaryOperator {
    /** Returns the result of invoking the wrapped Scala `Function2` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `sf`
     *  @param x2 the second argument to pass to `sf`
     */
    def applyAsInt(x1: scala.Int, x2: scala.Int) = sf.apply(x1, x2)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function2`, converting it to a Java `IntBinaryOperator`.
   *
   *  @param underlying the Scala `Function2` to convert
   */
  class RichFunction2AsIntBinaryOperator(private val underlying: scala.Function2[Int, Int, Int]) extends AnyVal {
    /** Returns a Java `IntBinaryOperator` that calls `underlying`, or, if `underlying` is a `FromJavaIntBinaryOperator`, the Java `IntBinaryOperator` that wrapper holds. */
    @inline def asJava: java.util.function.IntBinaryOperator = underlying match {
      case FromJavaIntBinaryOperator((jf @ _)) => jf.asInstanceOf[java.util.function.IntBinaryOperator]
      case _ => new AsJavaIntBinaryOperator(underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `IntConsumer`.
   *
   *  @param jf the Java `IntConsumer` to which `apply` delegates
   */
  case class FromJavaIntConsumer(jf: java.util.function.IntConsumer) extends scala.Function1[Int, Unit] {
    /** Invokes the wrapped Java `IntConsumer` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     *  @return `()`, since the delegate is called only for its side effect
     */
    def apply(x1: scala.Int) = jf.accept(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `IntConsumer`, converting it to a Scala `Function1`.
   *
   *  @param underlying the Java `IntConsumer` to convert
   */
  class RichIntConsumerAsFunction1(private val underlying: java.util.function.IntConsumer) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaIntConsumer`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Int, Unit] = underlying match {
      case AsJavaIntConsumer((sf @ _)) => sf.asInstanceOf[scala.Function1[Int, Unit]]
      case _ => new FromJavaIntConsumer(underlying)
    }
  }
  
  /** A Java `IntConsumer` that delegates to a Scala `Function1`.
   *
   *  @param sf the Scala `Function1` to which `accept` delegates
   */
  case class AsJavaIntConsumer(sf: scala.Function1[Int, Unit]) extends java.util.function.IntConsumer {
    /** Invokes the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     *  @return `()`, since the delegate is called only for its side effect
     */
    def accept(x1: scala.Int) = sf.apply(x1)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function1`, converting it to a Java `IntConsumer`.
   *
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsIntConsumer(private val underlying: scala.Function1[Int, Unit]) extends AnyVal {
    /** Returns a Java `IntConsumer` that calls `underlying`, or, if `underlying` is a `FromJavaIntConsumer`, the Java `IntConsumer` that wrapper holds. */
    @inline def asJava: java.util.function.IntConsumer = underlying match {
      case FromJavaIntConsumer((jf @ _)) => jf.asInstanceOf[java.util.function.IntConsumer]
      case _ => new AsJavaIntConsumer(underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `IntFunction`.
   *
   *  @tparam R the return type of the function
   *  @param jf the Java `IntFunction` to which `apply` delegates
   */
  case class FromJavaIntFunction[R](jf: java.util.function.IntFunction[R]) extends scala.Function1[Int, R] {
    /** Returns the result of invoking the wrapped Java `IntFunction` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: scala.Int) = jf.apply(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `IntFunction`, converting it to a Scala `Function1`.
   *
   *  @tparam R the return type of the function
   *  @param underlying the Java `IntFunction` to convert
   */
  class RichIntFunctionAsFunction1[R](private val underlying: java.util.function.IntFunction[R]) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaIntFunction`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Int, R] = underlying match {
      case AsJavaIntFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Int, R]]
      case _ => new FromJavaIntFunction[R](underlying)
    }
  }
  
  /** A Java `IntFunction` that delegates to a Scala `Function1`.
   *
   *  @tparam R the return type of the function
   *  @param sf the Scala `Function1` to which `apply` delegates
   */
  case class AsJavaIntFunction[R](sf: scala.Function1[Int, R]) extends java.util.function.IntFunction[R] {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def apply(x1: scala.Int): R = sf.apply(x1)
  }
  
  /** A value class that adds `asJava` and `asJavaIntFunction` methods to a Scala `Function1`, converting it to a Java `IntFunction`.
   *
   *  @tparam R the return type of the function
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsIntFunction[R](private val underlying: scala.Function1[Int, R]) extends AnyVal {
    /** Returns a Java `IntFunction` that calls `underlying`, or, if `underlying` is a `FromJavaIntFunction`, the Java `IntFunction` that wrapper holds. */
    @inline def asJava: java.util.function.IntFunction[R] = underlying match {
      case FromJavaIntFunction((jf @ _)) => jf.asInstanceOf[java.util.function.IntFunction[R]]
      case _ => new AsJavaIntFunction[R](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `IntFunction` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaIntFunction: java.util.function.IntFunction[R] = underlying match {
      case FromJavaIntFunction((sf @ _)) => sf.asInstanceOf[java.util.function.IntFunction[R]]
      case _ => new AsJavaIntFunction[R](underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `IntPredicate`.
   *
   *  @param jf the Java `IntPredicate` to which `apply` delegates
   */
  case class FromJavaIntPredicate(jf: java.util.function.IntPredicate) extends scala.Function1[Int, Boolean] {
    /** Returns the result of invoking the wrapped Java `IntPredicate` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: scala.Int) = jf.test(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `IntPredicate`, converting it to a Scala `Function1`.
   *
   *  @param underlying the Java `IntPredicate` to convert
   */
  class RichIntPredicateAsFunction1(private val underlying: java.util.function.IntPredicate) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaIntPredicate`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Int, Boolean] = underlying match {
      case AsJavaIntPredicate((sf @ _)) => sf.asInstanceOf[scala.Function1[Int, Boolean]]
      case _ => new FromJavaIntPredicate(underlying)
    }
  }
  
  /** A Java `IntPredicate` that delegates to a Scala `Function1`.
   *
   *  @param sf the Scala `Function1` to which `test` delegates
   */
  case class AsJavaIntPredicate(sf: scala.Function1[Int, Boolean]) extends java.util.function.IntPredicate {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def test(x1: scala.Int) = sf.apply(x1)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function1`, converting it to a Java `IntPredicate`.
   *
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsIntPredicate(private val underlying: scala.Function1[Int, Boolean]) extends AnyVal {
    /** Returns a Java `IntPredicate` that calls `underlying`, or, if `underlying` is a `FromJavaIntPredicate`, the Java `IntPredicate` that wrapper holds. */
    @inline def asJava: java.util.function.IntPredicate = underlying match {
      case FromJavaIntPredicate((jf @ _)) => jf.asInstanceOf[java.util.function.IntPredicate]
      case _ => new AsJavaIntPredicate(underlying)
    }
  }
  
  
  /** A Scala `Function0` that delegates to a Java `IntSupplier`.
   *
   *  @param jf the Java `IntSupplier` to which `apply` delegates
   */
  case class FromJavaIntSupplier(jf: java.util.function.IntSupplier) extends scala.Function0[Int] {
    /** Returns the value produced by the wrapped Java `IntSupplier`. */
    def apply() = jf.getAsInt()
  }
  
  /** A value class that adds an `asScala` method to a Java `IntSupplier`, converting it to a Scala `Function0`.
   *
   *  @param underlying the Java `IntSupplier` to convert
   */
  class RichIntSupplierAsFunction0(private val underlying: java.util.function.IntSupplier) extends AnyVal {
    /** Returns a Scala `Function0` that calls `underlying`, or, if `underlying` is an `AsJavaIntSupplier`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function0[Int] = underlying match {
      case AsJavaIntSupplier((sf @ _)) => sf.asInstanceOf[scala.Function0[Int]]
      case _ => new FromJavaIntSupplier(underlying)
    }
  }
  
  /** A Java `IntSupplier` that delegates to a Scala `Function0`.
   *
   *  @param sf the Scala `Function0` to which `getAsInt` delegates
   */
  case class AsJavaIntSupplier(sf: scala.Function0[Int]) extends java.util.function.IntSupplier {
    /** Returns the value produced by the wrapped Scala `Function0`. */
    def getAsInt() = sf.apply()
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function0`, converting it to a Java `IntSupplier`.
   *
   *  @param underlying the Scala `Function0` to convert
   */
  class RichFunction0AsIntSupplier(private val underlying: scala.Function0[Int]) extends AnyVal {
    /** Returns a Java `IntSupplier` that calls `underlying`, or, if `underlying` is a `FromJavaIntSupplier`, the Java `IntSupplier` that wrapper holds. */
    @inline def asJava: java.util.function.IntSupplier = underlying match {
      case FromJavaIntSupplier((jf @ _)) => jf.asInstanceOf[java.util.function.IntSupplier]
      case _ => new AsJavaIntSupplier(underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `IntToDoubleFunction`.
   *
   *  @param jf the Java `IntToDoubleFunction` to which `apply` delegates
   */
  case class FromJavaIntToDoubleFunction(jf: java.util.function.IntToDoubleFunction) extends scala.Function1[Int, Double] {
    /** Returns the result of invoking the wrapped Java `IntToDoubleFunction` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: scala.Int) = jf.applyAsDouble(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `IntToDoubleFunction`, converting it to a Scala `Function1`.
   *
   *  @param underlying the Java `IntToDoubleFunction` to convert
   */
  class RichIntToDoubleFunctionAsFunction1(private val underlying: java.util.function.IntToDoubleFunction) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaIntToDoubleFunction`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Int, Double] = underlying match {
      case AsJavaIntToDoubleFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Int, Double]]
      case _ => new FromJavaIntToDoubleFunction(underlying)
    }
  }
  
  /** A Java `IntToDoubleFunction` that delegates to a Scala `Function1`.
   *
   *  @param sf the Scala `Function1` to which `applyAsDouble` delegates
   */
  case class AsJavaIntToDoubleFunction(sf: scala.Function1[Int, Double]) extends java.util.function.IntToDoubleFunction {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def applyAsDouble(x1: scala.Int) = sf.apply(x1)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function1`, converting it to a Java `IntToDoubleFunction`.
   *
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsIntToDoubleFunction(private val underlying: scala.Function1[Int, Double]) extends AnyVal {
    /** Returns a Java `IntToDoubleFunction` that calls `underlying`, or, if `underlying` is a `FromJavaIntToDoubleFunction`, the Java `IntToDoubleFunction` that wrapper holds. */
    @inline def asJava: java.util.function.IntToDoubleFunction = underlying match {
      case FromJavaIntToDoubleFunction((jf @ _)) => jf.asInstanceOf[java.util.function.IntToDoubleFunction]
      case _ => new AsJavaIntToDoubleFunction(underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `IntToLongFunction`.
   *
   *  @param jf the Java `IntToLongFunction` to which `apply` delegates
   */
  case class FromJavaIntToLongFunction(jf: java.util.function.IntToLongFunction) extends scala.Function1[Int, Long] {
    /** Returns the result of invoking the wrapped Java `IntToLongFunction` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: scala.Int) = jf.applyAsLong(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `IntToLongFunction`, converting it to a Scala `Function1`.
   *
   *  @param underlying the Java `IntToLongFunction` to convert
   */
  class RichIntToLongFunctionAsFunction1(private val underlying: java.util.function.IntToLongFunction) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaIntToLongFunction`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Int, Long] = underlying match {
      case AsJavaIntToLongFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Int, Long]]
      case _ => new FromJavaIntToLongFunction(underlying)
    }
  }
  
  /** A Java `IntToLongFunction` that delegates to a Scala `Function1`.
   *
   *  @param sf the Scala `Function1` to which `applyAsLong` delegates
   */
  case class AsJavaIntToLongFunction(sf: scala.Function1[Int, Long]) extends java.util.function.IntToLongFunction {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def applyAsLong(x1: scala.Int) = sf.apply(x1)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function1`, converting it to a Java `IntToLongFunction`.
   *
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsIntToLongFunction(private val underlying: scala.Function1[Int, Long]) extends AnyVal {
    /** Returns a Java `IntToLongFunction` that calls `underlying`, or, if `underlying` is a `FromJavaIntToLongFunction`, the Java `IntToLongFunction` that wrapper holds. */
    @inline def asJava: java.util.function.IntToLongFunction = underlying match {
      case FromJavaIntToLongFunction((jf @ _)) => jf.asInstanceOf[java.util.function.IntToLongFunction]
      case _ => new AsJavaIntToLongFunction(underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `IntUnaryOperator`.
   *
   *  @param jf the Java `IntUnaryOperator` to which `apply` delegates
   */
  case class FromJavaIntUnaryOperator(jf: java.util.function.IntUnaryOperator) extends scala.Function1[Int, Int] {
    /** Returns the result of invoking the wrapped Java `IntUnaryOperator` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: scala.Int) = jf.applyAsInt(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `IntUnaryOperator`, converting it to a Scala `Function1`.
   *
   *  @param underlying the Java `IntUnaryOperator` to convert
   */
  class RichIntUnaryOperatorAsFunction1(private val underlying: java.util.function.IntUnaryOperator) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaIntUnaryOperator`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Int, Int] = underlying match {
      case AsJavaIntUnaryOperator((sf @ _)) => sf.asInstanceOf[scala.Function1[Int, Int]]
      case _ => new FromJavaIntUnaryOperator(underlying)
    }
  }
  
  /** A Java `IntUnaryOperator` that delegates to a Scala `Function1`.
   *
   *  @param sf the Scala `Function1` to which `applyAsInt` delegates
   */
  case class AsJavaIntUnaryOperator(sf: scala.Function1[Int, Int]) extends java.util.function.IntUnaryOperator {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def applyAsInt(x1: scala.Int) = sf.apply(x1)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function1`, converting it to a Java `IntUnaryOperator`.
   *
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsIntUnaryOperator(private val underlying: scala.Function1[Int, Int]) extends AnyVal {
    /** Returns a Java `IntUnaryOperator` that calls `underlying`, or, if `underlying` is a `FromJavaIntUnaryOperator`, the Java `IntUnaryOperator` that wrapper holds. */
    @inline def asJava: java.util.function.IntUnaryOperator = underlying match {
      case FromJavaIntUnaryOperator((jf @ _)) => jf.asInstanceOf[java.util.function.IntUnaryOperator]
      case _ => new AsJavaIntUnaryOperator(underlying)
    }
  }
  
  
  /** A Scala `Function2` that delegates to a Java `LongBinaryOperator`.
   *
   *  @param jf the Java `LongBinaryOperator` to which `apply` delegates
   */
  case class FromJavaLongBinaryOperator(jf: java.util.function.LongBinaryOperator) extends scala.Function2[Long, Long, Long] {
    /** Returns the result of invoking the wrapped Java `LongBinaryOperator` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `jf`
     *  @param x2 the second argument to pass to `jf`
     */
    def apply(x1: scala.Long, x2: scala.Long) = jf.applyAsLong(x1, x2)
  }
  
  /** A value class that adds an `asScala` method to a Java `LongBinaryOperator`, converting it to a Scala `Function2`.
   *
   *  @param underlying the Java `LongBinaryOperator` to convert
   */
  class RichLongBinaryOperatorAsFunction2(private val underlying: java.util.function.LongBinaryOperator) extends AnyVal {
    /** Returns a Scala `Function2` that calls `underlying`, or, if `underlying` is an `AsJavaLongBinaryOperator`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function2[Long, Long, Long] = underlying match {
      case AsJavaLongBinaryOperator((sf @ _)) => sf.asInstanceOf[scala.Function2[Long, Long, Long]]
      case _ => new FromJavaLongBinaryOperator(underlying)
    }
  }
  
  /** A Java `LongBinaryOperator` that delegates to a Scala `Function2`.
   *
   *  @param sf the Scala `Function2` to which `applyAsLong` delegates
   */
  case class AsJavaLongBinaryOperator(sf: scala.Function2[Long, Long, Long]) extends java.util.function.LongBinaryOperator {
    /** Returns the result of invoking the wrapped Scala `Function2` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `sf`
     *  @param x2 the second argument to pass to `sf`
     */
    def applyAsLong(x1: scala.Long, x2: scala.Long) = sf.apply(x1, x2)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function2`, converting it to a Java `LongBinaryOperator`.
   *
   *  @param underlying the Scala `Function2` to convert
   */
  class RichFunction2AsLongBinaryOperator(private val underlying: scala.Function2[Long, Long, Long]) extends AnyVal {
    /** Returns a Java `LongBinaryOperator` that calls `underlying`, or, if `underlying` is a `FromJavaLongBinaryOperator`, the Java `LongBinaryOperator` that wrapper holds. */
    @inline def asJava: java.util.function.LongBinaryOperator = underlying match {
      case FromJavaLongBinaryOperator((jf @ _)) => jf.asInstanceOf[java.util.function.LongBinaryOperator]
      case _ => new AsJavaLongBinaryOperator(underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `LongConsumer`.
   *
   *  @param jf the Java `LongConsumer` to which `apply` delegates
   */
  case class FromJavaLongConsumer(jf: java.util.function.LongConsumer) extends scala.Function1[Long, Unit] {
    /** Invokes the wrapped Java `LongConsumer` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     *  @return `()`, since the delegate is called only for its side effect
     */
    def apply(x1: scala.Long) = jf.accept(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `LongConsumer`, converting it to a Scala `Function1`.
   *
   *  @param underlying the Java `LongConsumer` to convert
   */
  class RichLongConsumerAsFunction1(private val underlying: java.util.function.LongConsumer) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaLongConsumer`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Long, Unit] = underlying match {
      case AsJavaLongConsumer((sf @ _)) => sf.asInstanceOf[scala.Function1[Long, Unit]]
      case _ => new FromJavaLongConsumer(underlying)
    }
  }
  
  /** A Java `LongConsumer` that delegates to a Scala `Function1`.
   *
   *  @param sf the Scala `Function1` to which `accept` delegates
   */
  case class AsJavaLongConsumer(sf: scala.Function1[Long, Unit]) extends java.util.function.LongConsumer {
    /** Invokes the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     *  @return `()`, since the delegate is called only for its side effect
     */
    def accept(x1: scala.Long) = sf.apply(x1)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function1`, converting it to a Java `LongConsumer`.
   *
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsLongConsumer(private val underlying: scala.Function1[Long, Unit]) extends AnyVal {
    /** Returns a Java `LongConsumer` that calls `underlying`, or, if `underlying` is a `FromJavaLongConsumer`, the Java `LongConsumer` that wrapper holds. */
    @inline def asJava: java.util.function.LongConsumer = underlying match {
      case FromJavaLongConsumer((jf @ _)) => jf.asInstanceOf[java.util.function.LongConsumer]
      case _ => new AsJavaLongConsumer(underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `LongFunction`.
   *
   *  @tparam R the return type of the function
   *  @param jf the Java `LongFunction` to which `apply` delegates
   */
  case class FromJavaLongFunction[R](jf: java.util.function.LongFunction[R]) extends scala.Function1[Long, R] {
    /** Returns the result of invoking the wrapped Java `LongFunction` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: scala.Long) = jf.apply(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `LongFunction`, converting it to a Scala `Function1`.
   *
   *  @tparam R the return type of the function
   *  @param underlying the Java `LongFunction` to convert
   */
  class RichLongFunctionAsFunction1[R](private val underlying: java.util.function.LongFunction[R]) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaLongFunction`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Long, R] = underlying match {
      case AsJavaLongFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Long, R]]
      case _ => new FromJavaLongFunction[R](underlying)
    }
  }
  
  /** A Java `LongFunction` that delegates to a Scala `Function1`.
   *
   *  @tparam R the return type of the function
   *  @param sf the Scala `Function1` to which `apply` delegates
   */
  case class AsJavaLongFunction[R](sf: scala.Function1[Long, R]) extends java.util.function.LongFunction[R] {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def apply(x1: scala.Long): R = sf.apply(x1)
  }
  
  /** A value class that adds `asJava` and `asJavaLongFunction` methods to a Scala `Function1`, converting it to a Java `LongFunction`.
   *
   *  @tparam R the return type of the function
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsLongFunction[R](private val underlying: scala.Function1[Long, R]) extends AnyVal {
    /** Returns a Java `LongFunction` that calls `underlying`, or, if `underlying` is a `FromJavaLongFunction`, the Java `LongFunction` that wrapper holds. */
    @inline def asJava: java.util.function.LongFunction[R] = underlying match {
      case FromJavaLongFunction((jf @ _)) => jf.asInstanceOf[java.util.function.LongFunction[R]]
      case _ => new AsJavaLongFunction[R](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `LongFunction` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaLongFunction: java.util.function.LongFunction[R] = underlying match {
      case FromJavaLongFunction((sf @ _)) => sf.asInstanceOf[java.util.function.LongFunction[R]]
      case _ => new AsJavaLongFunction[R](underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `LongPredicate`.
   *
   *  @param jf the Java `LongPredicate` to which `apply` delegates
   */
  case class FromJavaLongPredicate(jf: java.util.function.LongPredicate) extends scala.Function1[Long, Boolean] {
    /** Returns the result of invoking the wrapped Java `LongPredicate` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: scala.Long) = jf.test(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `LongPredicate`, converting it to a Scala `Function1`.
   *
   *  @param underlying the Java `LongPredicate` to convert
   */
  class RichLongPredicateAsFunction1(private val underlying: java.util.function.LongPredicate) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaLongPredicate`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Long, Boolean] = underlying match {
      case AsJavaLongPredicate((sf @ _)) => sf.asInstanceOf[scala.Function1[Long, Boolean]]
      case _ => new FromJavaLongPredicate(underlying)
    }
  }
  
  /** A Java `LongPredicate` that delegates to a Scala `Function1`.
   *
   *  @param sf the Scala `Function1` to which `test` delegates
   */
  case class AsJavaLongPredicate(sf: scala.Function1[Long, Boolean]) extends java.util.function.LongPredicate {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def test(x1: scala.Long) = sf.apply(x1)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function1`, converting it to a Java `LongPredicate`.
   *
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsLongPredicate(private val underlying: scala.Function1[Long, Boolean]) extends AnyVal {
    /** Returns a Java `LongPredicate` that calls `underlying`, or, if `underlying` is a `FromJavaLongPredicate`, the Java `LongPredicate` that wrapper holds. */
    @inline def asJava: java.util.function.LongPredicate = underlying match {
      case FromJavaLongPredicate((jf @ _)) => jf.asInstanceOf[java.util.function.LongPredicate]
      case _ => new AsJavaLongPredicate(underlying)
    }
  }
  
  
  /** A Scala `Function0` that delegates to a Java `LongSupplier`.
   *
   *  @param jf the Java `LongSupplier` to which `apply` delegates
   */
  case class FromJavaLongSupplier(jf: java.util.function.LongSupplier) extends scala.Function0[Long] {
    /** Returns the value produced by the wrapped Java `LongSupplier`. */
    def apply() = jf.getAsLong()
  }
  
  /** A value class that adds an `asScala` method to a Java `LongSupplier`, converting it to a Scala `Function0`.
   *
   *  @param underlying the Java `LongSupplier` to convert
   */
  class RichLongSupplierAsFunction0(private val underlying: java.util.function.LongSupplier) extends AnyVal {
    /** Returns a Scala `Function0` that calls `underlying`, or, if `underlying` is an `AsJavaLongSupplier`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function0[Long] = underlying match {
      case AsJavaLongSupplier((sf @ _)) => sf.asInstanceOf[scala.Function0[Long]]
      case _ => new FromJavaLongSupplier(underlying)
    }
  }
  
  /** A Java `LongSupplier` that delegates to a Scala `Function0`.
   *
   *  @param sf the Scala `Function0` to which `getAsLong` delegates
   */
  case class AsJavaLongSupplier(sf: scala.Function0[Long]) extends java.util.function.LongSupplier {
    /** Returns the value produced by the wrapped Scala `Function0`. */
    def getAsLong() = sf.apply()
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function0`, converting it to a Java `LongSupplier`.
   *
   *  @param underlying the Scala `Function0` to convert
   */
  class RichFunction0AsLongSupplier(private val underlying: scala.Function0[Long]) extends AnyVal {
    /** Returns a Java `LongSupplier` that calls `underlying`, or, if `underlying` is a `FromJavaLongSupplier`, the Java `LongSupplier` that wrapper holds. */
    @inline def asJava: java.util.function.LongSupplier = underlying match {
      case FromJavaLongSupplier((jf @ _)) => jf.asInstanceOf[java.util.function.LongSupplier]
      case _ => new AsJavaLongSupplier(underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `LongToDoubleFunction`.
   *
   *  @param jf the Java `LongToDoubleFunction` to which `apply` delegates
   */
  case class FromJavaLongToDoubleFunction(jf: java.util.function.LongToDoubleFunction) extends scala.Function1[Long, Double] {
    /** Returns the result of invoking the wrapped Java `LongToDoubleFunction` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: scala.Long) = jf.applyAsDouble(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `LongToDoubleFunction`, converting it to a Scala `Function1`.
   *
   *  @param underlying the Java `LongToDoubleFunction` to convert
   */
  class RichLongToDoubleFunctionAsFunction1(private val underlying: java.util.function.LongToDoubleFunction) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaLongToDoubleFunction`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Long, Double] = underlying match {
      case AsJavaLongToDoubleFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Long, Double]]
      case _ => new FromJavaLongToDoubleFunction(underlying)
    }
  }
  
  /** A Java `LongToDoubleFunction` that delegates to a Scala `Function1`.
   *
   *  @param sf the Scala `Function1` to which `applyAsDouble` delegates
   */
  case class AsJavaLongToDoubleFunction(sf: scala.Function1[Long, Double]) extends java.util.function.LongToDoubleFunction {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def applyAsDouble(x1: scala.Long) = sf.apply(x1)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function1`, converting it to a Java `LongToDoubleFunction`.
   *
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsLongToDoubleFunction(private val underlying: scala.Function1[Long, Double]) extends AnyVal {
    /** Returns a Java `LongToDoubleFunction` that calls `underlying`, or, if `underlying` is a `FromJavaLongToDoubleFunction`, the Java `LongToDoubleFunction` that wrapper holds. */
    @inline def asJava: java.util.function.LongToDoubleFunction = underlying match {
      case FromJavaLongToDoubleFunction((jf @ _)) => jf.asInstanceOf[java.util.function.LongToDoubleFunction]
      case _ => new AsJavaLongToDoubleFunction(underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `LongToIntFunction`.
   *
   *  @param jf the Java `LongToIntFunction` to which `apply` delegates
   */
  case class FromJavaLongToIntFunction(jf: java.util.function.LongToIntFunction) extends scala.Function1[Long, Int] {
    /** Returns the result of invoking the wrapped Java `LongToIntFunction` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: scala.Long) = jf.applyAsInt(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `LongToIntFunction`, converting it to a Scala `Function1`.
   *
   *  @param underlying the Java `LongToIntFunction` to convert
   */
  class RichLongToIntFunctionAsFunction1(private val underlying: java.util.function.LongToIntFunction) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaLongToIntFunction`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Long, Int] = underlying match {
      case AsJavaLongToIntFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[Long, Int]]
      case _ => new FromJavaLongToIntFunction(underlying)
    }
  }
  
  /** A Java `LongToIntFunction` that delegates to a Scala `Function1`.
   *
   *  @param sf the Scala `Function1` to which `applyAsInt` delegates
   */
  case class AsJavaLongToIntFunction(sf: scala.Function1[Long, Int]) extends java.util.function.LongToIntFunction {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def applyAsInt(x1: scala.Long) = sf.apply(x1)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function1`, converting it to a Java `LongToIntFunction`.
   *
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsLongToIntFunction(private val underlying: scala.Function1[Long, Int]) extends AnyVal {
    /** Returns a Java `LongToIntFunction` that calls `underlying`, or, if `underlying` is a `FromJavaLongToIntFunction`, the Java `LongToIntFunction` that wrapper holds. */
    @inline def asJava: java.util.function.LongToIntFunction = underlying match {
      case FromJavaLongToIntFunction((jf @ _)) => jf.asInstanceOf[java.util.function.LongToIntFunction]
      case _ => new AsJavaLongToIntFunction(underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `LongUnaryOperator`.
   *
   *  @param jf the Java `LongUnaryOperator` to which `apply` delegates
   */
  case class FromJavaLongUnaryOperator(jf: java.util.function.LongUnaryOperator) extends scala.Function1[Long, Long] {
    /** Returns the result of invoking the wrapped Java `LongUnaryOperator` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: scala.Long) = jf.applyAsLong(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `LongUnaryOperator`, converting it to a Scala `Function1`.
   *
   *  @param underlying the Java `LongUnaryOperator` to convert
   */
  class RichLongUnaryOperatorAsFunction1(private val underlying: java.util.function.LongUnaryOperator) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaLongUnaryOperator`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[Long, Long] = underlying match {
      case AsJavaLongUnaryOperator((sf @ _)) => sf.asInstanceOf[scala.Function1[Long, Long]]
      case _ => new FromJavaLongUnaryOperator(underlying)
    }
  }
  
  /** A Java `LongUnaryOperator` that delegates to a Scala `Function1`.
   *
   *  @param sf the Scala `Function1` to which `applyAsLong` delegates
   */
  case class AsJavaLongUnaryOperator(sf: scala.Function1[Long, Long]) extends java.util.function.LongUnaryOperator {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def applyAsLong(x1: scala.Long) = sf.apply(x1)
  }
  
  /** A value class that adds an `asJava` method to a Scala `Function1`, converting it to a Java `LongUnaryOperator`.
   *
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsLongUnaryOperator(private val underlying: scala.Function1[Long, Long]) extends AnyVal {
    /** Returns a Java `LongUnaryOperator` that calls `underlying`, or, if `underlying` is a `FromJavaLongUnaryOperator`, the Java `LongUnaryOperator` that wrapper holds. */
    @inline def asJava: java.util.function.LongUnaryOperator = underlying match {
      case FromJavaLongUnaryOperator((jf @ _)) => jf.asInstanceOf[java.util.function.LongUnaryOperator]
      case _ => new AsJavaLongUnaryOperator(underlying)
    }
  }
  
  
  /** A Scala `Function2` that delegates to a Java `ObjDoubleConsumer`.
   *
   *  @tparam T the type of the first (object) argument to the consumer (the second argument is a primitive)
   *  @param jf the Java `ObjDoubleConsumer` to which `apply` delegates
   */
  case class FromJavaObjDoubleConsumer[T](jf: java.util.function.ObjDoubleConsumer[T]) extends scala.Function2[T, Double, Unit] {
    /** Invokes the wrapped Java `ObjDoubleConsumer` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `jf`
     *  @param x2 the second argument to pass to `jf`
     *  @return `()`, since the delegate is called only for its side effect
     */
    def apply(x1: T, x2: scala.Double) = jf.accept(x1, x2)
  }
  
  /** A value class that adds an `asScala` method to a Java `ObjDoubleConsumer`, converting it to a Scala `Function2`.
   *
   *  @tparam T the type of the first (object) argument to the consumer (the second argument is a primitive)
   *  @param underlying the Java `ObjDoubleConsumer` to convert
   */
  class RichObjDoubleConsumerAsFunction2[T](private val underlying: java.util.function.ObjDoubleConsumer[T]) extends AnyVal {
    /** Returns a Scala `Function2` that calls `underlying`, or, if `underlying` is an `AsJavaObjDoubleConsumer`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function2[T, Double, Unit] = underlying match {
      case AsJavaObjDoubleConsumer((sf @ _)) => sf.asInstanceOf[scala.Function2[T, Double, Unit]]
      case _ => new FromJavaObjDoubleConsumer[T](underlying)
    }
  }
  
  /** A Java `ObjDoubleConsumer` that delegates to a Scala `Function2`.
   *
   *  @tparam T the type of the first (object) argument to the consumer (the second argument is a primitive)
   *  @param sf the Scala `Function2` to which `accept` delegates
   */
  case class AsJavaObjDoubleConsumer[T](sf: scala.Function2[T, Double, Unit]) extends java.util.function.ObjDoubleConsumer[T] {
    /** Invokes the wrapped Scala `Function2` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `sf`
     *  @param x2 the second argument to pass to `sf`
     *  @return `()`, since the delegate is called only for its side effect
     */
    def accept(x1: T, x2: scala.Double) = sf.apply(x1, x2)
  }
  
  /** A value class that adds `asJava` and `asJavaObjDoubleConsumer` methods to a Scala `Function2`, converting it to a Java `ObjDoubleConsumer`.
   *
   *  @tparam T the type of the first (object) argument to the consumer (the second argument is a primitive)
   *  @param underlying the Scala `Function2` to convert
   */
  class RichFunction2AsObjDoubleConsumer[T](private val underlying: scala.Function2[T, Double, Unit]) extends AnyVal {
    /** Returns a Java `ObjDoubleConsumer` that calls `underlying`, or, if `underlying` is a `FromJavaObjDoubleConsumer`, the Java `ObjDoubleConsumer` that wrapper holds. */
    @inline def asJava: java.util.function.ObjDoubleConsumer[T] = underlying match {
      case FromJavaObjDoubleConsumer((jf @ _)) => jf.asInstanceOf[java.util.function.ObjDoubleConsumer[T]]
      case _ => new AsJavaObjDoubleConsumer[T](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `ObjDoubleConsumer` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaObjDoubleConsumer: java.util.function.ObjDoubleConsumer[T] = underlying match {
      case FromJavaObjDoubleConsumer((sf @ _)) => sf.asInstanceOf[java.util.function.ObjDoubleConsumer[T]]
      case _ => new AsJavaObjDoubleConsumer[T](underlying)
    }
  }
  
  
  /** A Scala `Function2` that delegates to a Java `ObjIntConsumer`.
   *
   *  @tparam T the type of the first (object) argument to the consumer (the second argument is a primitive)
   *  @param jf the Java `ObjIntConsumer` to which `apply` delegates
   */
  case class FromJavaObjIntConsumer[T](jf: java.util.function.ObjIntConsumer[T]) extends scala.Function2[T, Int, Unit] {
    /** Invokes the wrapped Java `ObjIntConsumer` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `jf`
     *  @param x2 the second argument to pass to `jf`
     *  @return `()`, since the delegate is called only for its side effect
     */
    def apply(x1: T, x2: scala.Int) = jf.accept(x1, x2)
  }
  
  /** A value class that adds an `asScala` method to a Java `ObjIntConsumer`, converting it to a Scala `Function2`.
   *
   *  @tparam T the type of the first (object) argument to the consumer (the second argument is a primitive)
   *  @param underlying the Java `ObjIntConsumer` to convert
   */
  class RichObjIntConsumerAsFunction2[T](private val underlying: java.util.function.ObjIntConsumer[T]) extends AnyVal {
    /** Returns a Scala `Function2` that calls `underlying`, or, if `underlying` is an `AsJavaObjIntConsumer`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function2[T, Int, Unit] = underlying match {
      case AsJavaObjIntConsumer((sf @ _)) => sf.asInstanceOf[scala.Function2[T, Int, Unit]]
      case _ => new FromJavaObjIntConsumer[T](underlying)
    }
  }
  
  /** A Java `ObjIntConsumer` that delegates to a Scala `Function2`.
   *
   *  @tparam T the type of the first (object) argument to the consumer (the second argument is a primitive)
   *  @param sf the Scala `Function2` to which `accept` delegates
   */
  case class AsJavaObjIntConsumer[T](sf: scala.Function2[T, Int, Unit]) extends java.util.function.ObjIntConsumer[T] {
    /** Invokes the wrapped Scala `Function2` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `sf`
     *  @param x2 the second argument to pass to `sf`
     *  @return `()`, since the delegate is called only for its side effect
     */
    def accept(x1: T, x2: scala.Int) = sf.apply(x1, x2)
  }
  
  /** A value class that adds `asJava` and `asJavaObjIntConsumer` methods to a Scala `Function2`, converting it to a Java `ObjIntConsumer`.
   *
   *  @tparam T the type of the first (object) argument to the consumer (the second argument is a primitive)
   *  @param underlying the Scala `Function2` to convert
   */
  class RichFunction2AsObjIntConsumer[T](private val underlying: scala.Function2[T, Int, Unit]) extends AnyVal {
    /** Returns a Java `ObjIntConsumer` that calls `underlying`, or, if `underlying` is a `FromJavaObjIntConsumer`, the Java `ObjIntConsumer` that wrapper holds. */
    @inline def asJava: java.util.function.ObjIntConsumer[T] = underlying match {
      case FromJavaObjIntConsumer((jf @ _)) => jf.asInstanceOf[java.util.function.ObjIntConsumer[T]]
      case _ => new AsJavaObjIntConsumer[T](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `ObjIntConsumer` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaObjIntConsumer: java.util.function.ObjIntConsumer[T] = underlying match {
      case FromJavaObjIntConsumer((sf @ _)) => sf.asInstanceOf[java.util.function.ObjIntConsumer[T]]
      case _ => new AsJavaObjIntConsumer[T](underlying)
    }
  }
  
  
  /** A Scala `Function2` that delegates to a Java `ObjLongConsumer`.
   *
   *  @tparam T the type of the first (object) argument to the consumer (the second argument is a primitive)
   *  @param jf the Java `ObjLongConsumer` to which `apply` delegates
   */
  case class FromJavaObjLongConsumer[T](jf: java.util.function.ObjLongConsumer[T]) extends scala.Function2[T, Long, Unit] {
    /** Invokes the wrapped Java `ObjLongConsumer` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `jf`
     *  @param x2 the second argument to pass to `jf`
     *  @return `()`, since the delegate is called only for its side effect
     */
    def apply(x1: T, x2: scala.Long) = jf.accept(x1, x2)
  }
  
  /** A value class that adds an `asScala` method to a Java `ObjLongConsumer`, converting it to a Scala `Function2`.
   *
   *  @tparam T the type of the first (object) argument to the consumer (the second argument is a primitive)
   *  @param underlying the Java `ObjLongConsumer` to convert
   */
  class RichObjLongConsumerAsFunction2[T](private val underlying: java.util.function.ObjLongConsumer[T]) extends AnyVal {
    /** Returns a Scala `Function2` that calls `underlying`, or, if `underlying` is an `AsJavaObjLongConsumer`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function2[T, Long, Unit] = underlying match {
      case AsJavaObjLongConsumer((sf @ _)) => sf.asInstanceOf[scala.Function2[T, Long, Unit]]
      case _ => new FromJavaObjLongConsumer[T](underlying)
    }
  }
  
  /** A Java `ObjLongConsumer` that delegates to a Scala `Function2`.
   *
   *  @tparam T the type of the first (object) argument to the consumer (the second argument is a primitive)
   *  @param sf the Scala `Function2` to which `accept` delegates
   */
  case class AsJavaObjLongConsumer[T](sf: scala.Function2[T, Long, Unit]) extends java.util.function.ObjLongConsumer[T] {
    /** Invokes the wrapped Scala `Function2` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `sf`
     *  @param x2 the second argument to pass to `sf`
     *  @return `()`, since the delegate is called only for its side effect
     */
    def accept(x1: T, x2: scala.Long) = sf.apply(x1, x2)
  }
  
  /** A value class that adds `asJava` and `asJavaObjLongConsumer` methods to a Scala `Function2`, converting it to a Java `ObjLongConsumer`.
   *
   *  @tparam T the type of the first (object) argument to the consumer (the second argument is a primitive)
   *  @param underlying the Scala `Function2` to convert
   */
  class RichFunction2AsObjLongConsumer[T](private val underlying: scala.Function2[T, Long, Unit]) extends AnyVal {
    /** Returns a Java `ObjLongConsumer` that calls `underlying`, or, if `underlying` is a `FromJavaObjLongConsumer`, the Java `ObjLongConsumer` that wrapper holds. */
    @inline def asJava: java.util.function.ObjLongConsumer[T] = underlying match {
      case FromJavaObjLongConsumer((jf @ _)) => jf.asInstanceOf[java.util.function.ObjLongConsumer[T]]
      case _ => new AsJavaObjLongConsumer[T](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `ObjLongConsumer` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaObjLongConsumer: java.util.function.ObjLongConsumer[T] = underlying match {
      case FromJavaObjLongConsumer((sf @ _)) => sf.asInstanceOf[java.util.function.ObjLongConsumer[T]]
      case _ => new AsJavaObjLongConsumer[T](underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `Predicate`.
   *
   *  @tparam T the input type of the predicate
   *  @param jf the Java `Predicate` to which `apply` delegates
   */
  case class FromJavaPredicate[T](jf: java.util.function.Predicate[T]) extends scala.Function1[T, Boolean] {
    /** Returns the result of invoking the wrapped Java `Predicate` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: T) = jf.test(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `Predicate`, converting it to a Scala `Function1`.
   *
   *  @tparam T the input type of the predicate
   *  @param underlying the Java `Predicate` to convert
   */
  class RichPredicateAsFunction1[T](private val underlying: java.util.function.Predicate[T]) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaPredicate`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[T, Boolean] = underlying match {
      case AsJavaPredicate((sf @ _)) => sf.asInstanceOf[scala.Function1[T, Boolean]]
      case _ => new FromJavaPredicate[T](underlying)
    }
  }
  
  /** A Java `Predicate` that delegates to a Scala `Function1`.
   *
   *  @tparam T the input type of the predicate
   *  @param sf the Scala `Function1` to which `test` delegates
   */
  case class AsJavaPredicate[T](sf: scala.Function1[T, Boolean]) extends java.util.function.Predicate[T] {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def test(x1: T) = sf.apply(x1)
  }
  
  /** A value class that adds `asJava` and `asJavaPredicate` methods to a Scala `Function1`, converting it to a Java `Predicate`.
   *
   *  @tparam T the input type of the predicate
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsPredicate[T](private val underlying: scala.Function1[T, Boolean]) extends AnyVal {
    /** Returns a Java `Predicate` that calls `underlying`, or, if `underlying` is a `FromJavaPredicate`, the Java `Predicate` that wrapper holds. */
    @inline def asJava: java.util.function.Predicate[T] = underlying match {
      case FromJavaPredicate((jf @ _)) => jf.asInstanceOf[java.util.function.Predicate[T]]
      case _ => new AsJavaPredicate[T](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `Predicate` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaPredicate: java.util.function.Predicate[T] = underlying match {
      case FromJavaPredicate((sf @ _)) => sf.asInstanceOf[java.util.function.Predicate[T]]
      case _ => new AsJavaPredicate[T](underlying)
    }
  }
  
  
  /** A Scala `Function0` that delegates to a Java `Supplier`.
   *
   *  @tparam T the return type of the supplier
   *  @param jf the Java `Supplier` to which `apply` delegates
   */
  case class FromJavaSupplier[T](jf: java.util.function.Supplier[T]) extends scala.Function0[T] {
    /** Returns the value produced by the wrapped Java `Supplier`. */
    def apply() = jf.get()
  }
  
  /** A value class that adds an `asScala` method to a Java `Supplier`, converting it to a Scala `Function0`.
   *
   *  @tparam T the return type of the supplier
   *  @param underlying the Java `Supplier` to convert
   */
  class RichSupplierAsFunction0[T](private val underlying: java.util.function.Supplier[T]) extends AnyVal {
    /** Returns a Scala `Function0` that calls `underlying`, or, if `underlying` is an `AsJavaSupplier`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function0[T] = underlying match {
      case AsJavaSupplier((sf @ _)) => sf.asInstanceOf[scala.Function0[T]]
      case _ => new FromJavaSupplier[T](underlying)
    }
  }
  
  /** A Java `Supplier` that delegates to a Scala `Function0`.
   *
   *  @tparam T the return type of the supplier
   *  @param sf the Scala `Function0` to which `get` delegates
   */
  case class AsJavaSupplier[T](sf: scala.Function0[T]) extends java.util.function.Supplier[T] {
    /** Returns the value produced by the wrapped Scala `Function0`. */
    def get(): T = sf.apply()
  }
  
  /** A value class that adds `asJava` and `asJavaSupplier` methods to a Scala `Function0`, converting it to a Java `Supplier`.
   *
   *  @tparam T the return type of the supplier
   *  @param underlying the Scala `Function0` to convert
   */
  class RichFunction0AsSupplier[T](private val underlying: scala.Function0[T]) extends AnyVal {
    /** Returns a Java `Supplier` that calls `underlying`, or, if `underlying` is a `FromJavaSupplier`, the Java `Supplier` that wrapper holds. */
    @inline def asJava: java.util.function.Supplier[T] = underlying match {
      case FromJavaSupplier((jf @ _)) => jf.asInstanceOf[java.util.function.Supplier[T]]
      case _ => new AsJavaSupplier[T](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `Supplier` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaSupplier: java.util.function.Supplier[T] = underlying match {
      case FromJavaSupplier((sf @ _)) => sf.asInstanceOf[java.util.function.Supplier[T]]
      case _ => new AsJavaSupplier[T](underlying)
    }
  }
  
  
  /** A Scala `Function2` that delegates to a Java `ToDoubleBiFunction`.
   *
   *  @tparam T the first input type of the function
   *  @tparam U the second input type of the function
   *  @param jf the Java `ToDoubleBiFunction` to which `apply` delegates
   */
  case class FromJavaToDoubleBiFunction[T, U](jf: java.util.function.ToDoubleBiFunction[T, U]) extends scala.Function2[T, U, Double] {
    /** Returns the result of invoking the wrapped Java `ToDoubleBiFunction` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `jf`
     *  @param x2 the second argument to pass to `jf`
     */
    def apply(x1: T, x2: U) = jf.applyAsDouble(x1, x2)
  }
  
  /** A value class that adds an `asScala` method to a Java `ToDoubleBiFunction`, converting it to a Scala `Function2`.
   *
   *  @tparam T the first input type of the function
   *  @tparam U the second input type of the function
   *  @param underlying the Java `ToDoubleBiFunction` to convert
   */
  class RichToDoubleBiFunctionAsFunction2[T, U](private val underlying: java.util.function.ToDoubleBiFunction[T, U]) extends AnyVal {
    /** Returns a Scala `Function2` that calls `underlying`, or, if `underlying` is an `AsJavaToDoubleBiFunction`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function2[T, U, Double] = underlying match {
      case AsJavaToDoubleBiFunction((sf @ _)) => sf.asInstanceOf[scala.Function2[T, U, Double]]
      case _ => new FromJavaToDoubleBiFunction[T, U](underlying)
    }
  }
  
  /** A Java `ToDoubleBiFunction` that delegates to a Scala `Function2`.
   *
   *  @tparam T the first input type of the function
   *  @tparam U the second input type of the function
   *  @param sf the Scala `Function2` to which `applyAsDouble` delegates
   */
  case class AsJavaToDoubleBiFunction[T, U](sf: scala.Function2[T, U, Double]) extends java.util.function.ToDoubleBiFunction[T, U] {
    /** Returns the result of invoking the wrapped Scala `Function2` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `sf`
     *  @param x2 the second argument to pass to `sf`
     */
    def applyAsDouble(x1: T, x2: U) = sf.apply(x1, x2)
  }
  
  /** A value class that adds `asJava` and `asJavaToDoubleBiFunction` methods to a Scala `Function2`, converting it to a Java `ToDoubleBiFunction`.
   *
   *  @tparam T the first input type of the function
   *  @tparam U the second input type of the function
   *  @param underlying the Scala `Function2` to convert
   */
  class RichFunction2AsToDoubleBiFunction[T, U](private val underlying: scala.Function2[T, U, Double]) extends AnyVal {
    /** Returns a Java `ToDoubleBiFunction` that calls `underlying`, or, if `underlying` is a `FromJavaToDoubleBiFunction`, the Java `ToDoubleBiFunction` that wrapper holds. */
    @inline def asJava: java.util.function.ToDoubleBiFunction[T, U] = underlying match {
      case FromJavaToDoubleBiFunction((jf @ _)) => jf.asInstanceOf[java.util.function.ToDoubleBiFunction[T, U]]
      case _ => new AsJavaToDoubleBiFunction[T, U](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `ToDoubleBiFunction` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaToDoubleBiFunction: java.util.function.ToDoubleBiFunction[T, U] = underlying match {
      case FromJavaToDoubleBiFunction((sf @ _)) => sf.asInstanceOf[java.util.function.ToDoubleBiFunction[T, U]]
      case _ => new AsJavaToDoubleBiFunction[T, U](underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `ToDoubleFunction`.
   *
   *  @tparam T the input type of the function
   *  @param jf the Java `ToDoubleFunction` to which `apply` delegates
   */
  case class FromJavaToDoubleFunction[T](jf: java.util.function.ToDoubleFunction[T]) extends scala.Function1[T, Double] {
    /** Returns the result of invoking the wrapped Java `ToDoubleFunction` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: T) = jf.applyAsDouble(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `ToDoubleFunction`, converting it to a Scala `Function1`.
   *
   *  @tparam T the input type of the function
   *  @param underlying the Java `ToDoubleFunction` to convert
   */
  class RichToDoubleFunctionAsFunction1[T](private val underlying: java.util.function.ToDoubleFunction[T]) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaToDoubleFunction`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[T, Double] = underlying match {
      case AsJavaToDoubleFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[T, Double]]
      case _ => new FromJavaToDoubleFunction[T](underlying)
    }
  }
  
  /** A Java `ToDoubleFunction` that delegates to a Scala `Function1`.
   *
   *  @tparam T the input type of the function
   *  @param sf the Scala `Function1` to which `applyAsDouble` delegates
   */
  case class AsJavaToDoubleFunction[T](sf: scala.Function1[T, Double]) extends java.util.function.ToDoubleFunction[T] {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def applyAsDouble(x1: T) = sf.apply(x1)
  }
  
  /** A value class that adds `asJava` and `asJavaToDoubleFunction` methods to a Scala `Function1`, converting it to a Java `ToDoubleFunction`.
   *
   *  @tparam T the input type of the function
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsToDoubleFunction[T](private val underlying: scala.Function1[T, Double]) extends AnyVal {
    /** Returns a Java `ToDoubleFunction` that calls `underlying`, or, if `underlying` is a `FromJavaToDoubleFunction`, the Java `ToDoubleFunction` that wrapper holds. */
    @inline def asJava: java.util.function.ToDoubleFunction[T] = underlying match {
      case FromJavaToDoubleFunction((jf @ _)) => jf.asInstanceOf[java.util.function.ToDoubleFunction[T]]
      case _ => new AsJavaToDoubleFunction[T](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `ToDoubleFunction` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaToDoubleFunction: java.util.function.ToDoubleFunction[T] = underlying match {
      case FromJavaToDoubleFunction((sf @ _)) => sf.asInstanceOf[java.util.function.ToDoubleFunction[T]]
      case _ => new AsJavaToDoubleFunction[T](underlying)
    }
  }
  
  
  /** A Scala `Function2` that delegates to a Java `ToIntBiFunction`.
   *
   *  @tparam T the first input type of the function
   *  @tparam U the second input type of the function
   *  @param jf the Java `ToIntBiFunction` to which `apply` delegates
   */
  case class FromJavaToIntBiFunction[T, U](jf: java.util.function.ToIntBiFunction[T, U]) extends scala.Function2[T, U, Int] {
    /** Returns the result of invoking the wrapped Java `ToIntBiFunction` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `jf`
     *  @param x2 the second argument to pass to `jf`
     */
    def apply(x1: T, x2: U) = jf.applyAsInt(x1, x2)
  }
  
  /** A value class that adds an `asScala` method to a Java `ToIntBiFunction`, converting it to a Scala `Function2`.
   *
   *  @tparam T the first input type of the function
   *  @tparam U the second input type of the function
   *  @param underlying the Java `ToIntBiFunction` to convert
   */
  class RichToIntBiFunctionAsFunction2[T, U](private val underlying: java.util.function.ToIntBiFunction[T, U]) extends AnyVal {
    /** Returns a Scala `Function2` that calls `underlying`, or, if `underlying` is an `AsJavaToIntBiFunction`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function2[T, U, Int] = underlying match {
      case AsJavaToIntBiFunction((sf @ _)) => sf.asInstanceOf[scala.Function2[T, U, Int]]
      case _ => new FromJavaToIntBiFunction[T, U](underlying)
    }
  }
  
  /** A Java `ToIntBiFunction` that delegates to a Scala `Function2`.
   *
   *  @tparam T the first input type of the function
   *  @tparam U the second input type of the function
   *  @param sf the Scala `Function2` to which `applyAsInt` delegates
   */
  case class AsJavaToIntBiFunction[T, U](sf: scala.Function2[T, U, Int]) extends java.util.function.ToIntBiFunction[T, U] {
    /** Returns the result of invoking the wrapped Scala `Function2` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `sf`
     *  @param x2 the second argument to pass to `sf`
     */
    def applyAsInt(x1: T, x2: U) = sf.apply(x1, x2)
  }
  
  /** A value class that adds `asJava` and `asJavaToIntBiFunction` methods to a Scala `Function2`, converting it to a Java `ToIntBiFunction`.
   *
   *  @tparam T the first input type of the function
   *  @tparam U the second input type of the function
   *  @param underlying the Scala `Function2` to convert
   */
  class RichFunction2AsToIntBiFunction[T, U](private val underlying: scala.Function2[T, U, Int]) extends AnyVal {
    /** Returns a Java `ToIntBiFunction` that calls `underlying`, or, if `underlying` is a `FromJavaToIntBiFunction`, the Java `ToIntBiFunction` that wrapper holds. */
    @inline def asJava: java.util.function.ToIntBiFunction[T, U] = underlying match {
      case FromJavaToIntBiFunction((jf @ _)) => jf.asInstanceOf[java.util.function.ToIntBiFunction[T, U]]
      case _ => new AsJavaToIntBiFunction[T, U](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `ToIntBiFunction` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaToIntBiFunction: java.util.function.ToIntBiFunction[T, U] = underlying match {
      case FromJavaToIntBiFunction((sf @ _)) => sf.asInstanceOf[java.util.function.ToIntBiFunction[T, U]]
      case _ => new AsJavaToIntBiFunction[T, U](underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `ToIntFunction`.
   *
   *  @tparam T the input type of the function
   *  @param jf the Java `ToIntFunction` to which `apply` delegates
   */
  case class FromJavaToIntFunction[T](jf: java.util.function.ToIntFunction[T]) extends scala.Function1[T, Int] {
    /** Returns the result of invoking the wrapped Java `ToIntFunction` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: T) = jf.applyAsInt(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `ToIntFunction`, converting it to a Scala `Function1`.
   *
   *  @tparam T the input type of the function
   *  @param underlying the Java `ToIntFunction` to convert
   */
  class RichToIntFunctionAsFunction1[T](private val underlying: java.util.function.ToIntFunction[T]) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaToIntFunction`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[T, Int] = underlying match {
      case AsJavaToIntFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[T, Int]]
      case _ => new FromJavaToIntFunction[T](underlying)
    }
  }
  
  /** A Java `ToIntFunction` that delegates to a Scala `Function1`.
   *
   *  @tparam T the input type of the function
   *  @param sf the Scala `Function1` to which `applyAsInt` delegates
   */
  case class AsJavaToIntFunction[T](sf: scala.Function1[T, Int]) extends java.util.function.ToIntFunction[T] {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def applyAsInt(x1: T) = sf.apply(x1)
  }
  
  /** A value class that adds `asJava` and `asJavaToIntFunction` methods to a Scala `Function1`, converting it to a Java `ToIntFunction`.
   *
   *  @tparam T the input type of the function
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsToIntFunction[T](private val underlying: scala.Function1[T, Int]) extends AnyVal {
    /** Returns a Java `ToIntFunction` that calls `underlying`, or, if `underlying` is a `FromJavaToIntFunction`, the Java `ToIntFunction` that wrapper holds. */
    @inline def asJava: java.util.function.ToIntFunction[T] = underlying match {
      case FromJavaToIntFunction((jf @ _)) => jf.asInstanceOf[java.util.function.ToIntFunction[T]]
      case _ => new AsJavaToIntFunction[T](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `ToIntFunction` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaToIntFunction: java.util.function.ToIntFunction[T] = underlying match {
      case FromJavaToIntFunction((sf @ _)) => sf.asInstanceOf[java.util.function.ToIntFunction[T]]
      case _ => new AsJavaToIntFunction[T](underlying)
    }
  }
  
  
  /** A Scala `Function2` that delegates to a Java `ToLongBiFunction`.
   *
   *  @tparam T the first input type of the function
   *  @tparam U the second input type of the function
   *  @param jf the Java `ToLongBiFunction` to which `apply` delegates
   */
  case class FromJavaToLongBiFunction[T, U](jf: java.util.function.ToLongBiFunction[T, U]) extends scala.Function2[T, U, Long] {
    /** Returns the result of invoking the wrapped Java `ToLongBiFunction` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `jf`
     *  @param x2 the second argument to pass to `jf`
     */
    def apply(x1: T, x2: U) = jf.applyAsLong(x1, x2)
  }
  
  /** A value class that adds an `asScala` method to a Java `ToLongBiFunction`, converting it to a Scala `Function2`.
   *
   *  @tparam T the first input type of the function
   *  @tparam U the second input type of the function
   *  @param underlying the Java `ToLongBiFunction` to convert
   */
  class RichToLongBiFunctionAsFunction2[T, U](private val underlying: java.util.function.ToLongBiFunction[T, U]) extends AnyVal {
    /** Returns a Scala `Function2` that calls `underlying`, or, if `underlying` is an `AsJavaToLongBiFunction`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function2[T, U, Long] = underlying match {
      case AsJavaToLongBiFunction((sf @ _)) => sf.asInstanceOf[scala.Function2[T, U, Long]]
      case _ => new FromJavaToLongBiFunction[T, U](underlying)
    }
  }
  
  /** A Java `ToLongBiFunction` that delegates to a Scala `Function2`.
   *
   *  @tparam T the first input type of the function
   *  @tparam U the second input type of the function
   *  @param sf the Scala `Function2` to which `applyAsLong` delegates
   */
  case class AsJavaToLongBiFunction[T, U](sf: scala.Function2[T, U, Long]) extends java.util.function.ToLongBiFunction[T, U] {
    /** Returns the result of invoking the wrapped Scala `Function2` with `x1` and `x2`.
     *
     *  @param x1 the first argument to pass to `sf`
     *  @param x2 the second argument to pass to `sf`
     */
    def applyAsLong(x1: T, x2: U) = sf.apply(x1, x2)
  }
  
  /** A value class that adds `asJava` and `asJavaToLongBiFunction` methods to a Scala `Function2`, converting it to a Java `ToLongBiFunction`.
   *
   *  @tparam T the first input type of the function
   *  @tparam U the second input type of the function
   *  @param underlying the Scala `Function2` to convert
   */
  class RichFunction2AsToLongBiFunction[T, U](private val underlying: scala.Function2[T, U, Long]) extends AnyVal {
    /** Returns a Java `ToLongBiFunction` that calls `underlying`, or, if `underlying` is a `FromJavaToLongBiFunction`, the Java `ToLongBiFunction` that wrapper holds. */
    @inline def asJava: java.util.function.ToLongBiFunction[T, U] = underlying match {
      case FromJavaToLongBiFunction((jf @ _)) => jf.asInstanceOf[java.util.function.ToLongBiFunction[T, U]]
      case _ => new AsJavaToLongBiFunction[T, U](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `ToLongBiFunction` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaToLongBiFunction: java.util.function.ToLongBiFunction[T, U] = underlying match {
      case FromJavaToLongBiFunction((sf @ _)) => sf.asInstanceOf[java.util.function.ToLongBiFunction[T, U]]
      case _ => new AsJavaToLongBiFunction[T, U](underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `ToLongFunction`.
   *
   *  @tparam T the input type of the function
   *  @param jf the Java `ToLongFunction` to which `apply` delegates
   */
  case class FromJavaToLongFunction[T](jf: java.util.function.ToLongFunction[T]) extends scala.Function1[T, Long] {
    /** Returns the result of invoking the wrapped Java `ToLongFunction` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: T) = jf.applyAsLong(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `ToLongFunction`, converting it to a Scala `Function1`.
   *
   *  @tparam T the input type of the function
   *  @param underlying the Java `ToLongFunction` to convert
   */
  class RichToLongFunctionAsFunction1[T](private val underlying: java.util.function.ToLongFunction[T]) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaToLongFunction`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[T, Long] = underlying match {
      case AsJavaToLongFunction((sf @ _)) => sf.asInstanceOf[scala.Function1[T, Long]]
      case _ => new FromJavaToLongFunction[T](underlying)
    }
  }
  
  /** A Java `ToLongFunction` that delegates to a Scala `Function1`.
   *
   *  @tparam T the input type of the function
   *  @param sf the Scala `Function1` to which `applyAsLong` delegates
   */
  case class AsJavaToLongFunction[T](sf: scala.Function1[T, Long]) extends java.util.function.ToLongFunction[T] {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def applyAsLong(x1: T) = sf.apply(x1)
  }
  
  /** A value class that adds `asJava` and `asJavaToLongFunction` methods to a Scala `Function1`, converting it to a Java `ToLongFunction`.
   *
   *  @tparam T the input type of the function
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsToLongFunction[T](private val underlying: scala.Function1[T, Long]) extends AnyVal {
    /** Returns a Java `ToLongFunction` that calls `underlying`, or, if `underlying` is a `FromJavaToLongFunction`, the Java `ToLongFunction` that wrapper holds. */
    @inline def asJava: java.util.function.ToLongFunction[T] = underlying match {
      case FromJavaToLongFunction((jf @ _)) => jf.asInstanceOf[java.util.function.ToLongFunction[T]]
      case _ => new AsJavaToLongFunction[T](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `ToLongFunction` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaToLongFunction: java.util.function.ToLongFunction[T] = underlying match {
      case FromJavaToLongFunction((sf @ _)) => sf.asInstanceOf[java.util.function.ToLongFunction[T]]
      case _ => new AsJavaToLongFunction[T](underlying)
    }
  }
  
  
  /** A Scala `Function1` that delegates to a Java `UnaryOperator`.
   *
   *  @tparam T the input and output type of the unary operator
   *  @param jf the Java `UnaryOperator` to which `apply` delegates
   */
  case class FromJavaUnaryOperator[T](jf: java.util.function.UnaryOperator[T]) extends scala.Function1[T, T] {
    /** Returns the result of invoking the wrapped Java `UnaryOperator` with `x1`.
     *
     *  @param x1 the argument to pass to `jf`
     */
    def apply(x1: T): T = jf.apply(x1)
  }
  
  /** A value class that adds an `asScala` method to a Java `UnaryOperator`, converting it to a Scala `Function1`.
   *
   *  @tparam T the input and output type of the unary operator
   *  @param underlying the Java `UnaryOperator` to convert
   */
  class RichUnaryOperatorAsFunction1[T](private val underlying: java.util.function.UnaryOperator[T]) extends AnyVal {
    /** Returns a Scala `Function1` that calls `underlying`, or, if `underlying` is an `AsJavaUnaryOperator`, the Scala function that wrapper holds. */
    @inline def asScala: scala.Function1[T, T] = underlying match {
      case AsJavaUnaryOperator((sf @ _)) => sf.asInstanceOf[scala.Function1[T, T]]
      case _ => new FromJavaUnaryOperator[T](underlying)
    }
  }
  
  /** A Java `UnaryOperator` that delegates to a Scala `Function1`.
   *
   *  @tparam T the input and output type of the unary operator
   *  @param sf the Scala `Function1` to which `apply` delegates
   */
  case class AsJavaUnaryOperator[T](sf: scala.Function1[T, T]) extends java.util.function.UnaryOperator[T] {
    /** Returns the result of invoking the wrapped Scala `Function1` with `x1`.
     *
     *  @param x1 the argument to pass to `sf`
     */
    def apply(x1: T): T = sf.apply(x1)
  }
  
  /** A value class that adds `asJava` and `asJavaUnaryOperator` methods to a Scala `Function1`, converting it to a Java `UnaryOperator`.
   *
   *  @tparam T the input and output type of the unary operator
   *  @param underlying the Scala `Function1` to convert
   */
  class RichFunction1AsUnaryOperator[T](private val underlying: scala.Function1[T, T]) extends AnyVal {
    /** Returns a Java `UnaryOperator` that calls `underlying`, or, if `underlying` is a `FromJavaUnaryOperator`, the Java `UnaryOperator` that wrapper holds. */
    @inline def asJava: java.util.function.UnaryOperator[T] = underlying match {
      case FromJavaUnaryOperator((jf @ _)) => jf.asInstanceOf[java.util.function.UnaryOperator[T]]
      case _ => new AsJavaUnaryOperator[T](underlying)
    };
    /** An explicitly named alias for `asJava`, with identical behavior.
     *
     *  @return a Java `UnaryOperator` that behaves identically to the one `asJava` returns
     */
    @inline def asJavaUnaryOperator: java.util.function.UnaryOperator[T] = underlying match {
      case FromJavaUnaryOperator((sf @ _)) => sf.asInstanceOf[java.util.function.UnaryOperator[T]]
      case _ => new AsJavaUnaryOperator[T](underlying)
    }
  }
}
