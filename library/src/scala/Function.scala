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

import scala.language.`2.13`
import scala.annotation.{experimental, unused}

/** A module defining utility methods for higher-order functional programming. */
object Function {
  /** Given a sequence of functions `f,,1,,`, ..., `f,,n,,`, return the
   *  function `f,,1,, andThen ... andThen f,,n,,`.
   *
   *  @tparam T the common input and output type of the functions in the chain
   *  @param fs the given sequence of functions
   *  @return the composed function that applies each function in `fs` left-to-right, or the identity function if `fs` is empty
   */
  def chain[T](fs: scala.collection.Seq[T => T]): T => T = { x => fs.foldLeft(x)((x, f) => f(x)) }

  /** The constant function.
   *
   *  @tparam T the return type
   *  @tparam U the type of the argument to ignore
   *  @param x the constant value to return
   *  @param y the argument that is ignored
   *  @return `x` regardless of the value of `y`
   */
  def const[T, U](x: T)(@unused y: U): T = x

  /** Turns a function `A => Option[B]` into a `PartialFunction[A, B]`.
   *
   *  **Important note**: this transformation implies the original function
   *  may be called 2 or more times on each logical invocation, because the
   *  only way to supply an implementation of `isDefinedAt` is to call the
   *  function and examine the return value.
   *  See also [[scala.PartialFunction]], method `applyOrElse`.
   *
   *  @tparam T the input type of the function
   *  @tparam R the result type
   *  @param   f    a function `T => Option[R]`
   *  @return       a partial function defined for those inputs where
   *                f returns `Some(_)` and undefined where `f` returns `None`.
   *  @see [[scala.PartialFunction]], method `lift`.
   */
  def unlift[T, R](f: T => Option[R]): PartialFunction[T, R] = PartialFunction.unlifted(f)

  /** Uncurrying for functions of arity 2. This transforms a unary function
   *  returning another unary function into a function of arity 2.
   *
   *  @tparam T1 the type of the 1st argument
   *  @tparam T2 the type of the 2nd argument
   *  @tparam R the result type
   *  @param f the curried function to uncurry
   *  @return an equivalent function of arity 2 that takes all arguments at once
   */
  def uncurried[T1, T2, R](f: T1 => T2 => R): (T1, T2) => R = {
    (x1, x2) => f(x1)(x2)
  }

  /** Uncurrying for functions of arity 3.
   *
   *  @tparam T1 the type of the 1st argument
   *  @tparam T2 the type of the 2nd argument
   *  @tparam T3 the type of the 3rd argument
   *  @tparam R the result type
   *  @param f the curried function to uncurry
   *  @return an equivalent function of arity 3 that takes all arguments at once
   */
  def uncurried[T1, T2, T3, R](f: T1 => T2 => T3 => R): (T1, T2, T3) => R = {
    (x1, x2, x3) => f(x1)(x2)(x3)
  }

  /** Uncurrying for functions of arity 4.
   *
   *  @tparam T1 the type of the 1st argument
   *  @tparam T2 the type of the 2nd argument
   *  @tparam T3 the type of the 3rd argument
   *  @tparam T4 the type of the 4th argument
   *  @tparam R the result type
   *  @param f the curried function to uncurry
   *  @return an equivalent function of arity 4 that takes all arguments at once
   */
  def uncurried[T1, T2, T3, T4, R](f: T1 => T2 => T3 => T4 => R): (T1, T2, T3, T4) => R = {
    (x1, x2, x3, x4) => f(x1)(x2)(x3)(x4)
  }

  /** Uncurrying for functions of arity 5.
   *
   *  @tparam T1 the type of the 1st argument
   *  @tparam T2 the type of the 2nd argument
   *  @tparam T3 the type of the 3rd argument
   *  @tparam T4 the type of the 4th argument
   *  @tparam T5 the type of the 5th argument
   *  @tparam R the result type
   *  @param f the curried function to uncurry
   *  @return an equivalent function of arity 5 that takes all arguments at once
   */
  def uncurried[T1, T2, T3, T4, T5, R](f: T1 => T2 => T3 => T4 => T5 => R): (T1, T2, T3, T4, T5) => R  =  {
    (x1, x2, x3, x4, x5) => f(x1)(x2)(x3)(x4)(x5)
  }

  /** Tupling for functions of arity 2. This transforms a function
   *  of arity 2 into a unary function that takes a pair of arguments.
   *
   *  @note  These functions are slotted for deprecation, but it is on
   *  hold pending superior type inference for tupling anonymous functions.
   */
  // @deprecated("use `f.tupled` instead")
  def tupled[T1, T2, R](f: (T1, T2) => R): ((T1, T2)) => R = {
    case ((x1, x2)) => f(x1, x2)
  }

  /** Tupling for functions of arity 3. This transforms a function
   *  of arity 3 into a unary function that takes a triple of arguments.
   */
  // @deprecated("use `f.tupled` instead")
  def tupled[T1, T2, T3, R](f: (T1, T2, T3) => R): ((T1, T2, T3)) => R = {
    case ((x1, x2, x3)) => f(x1, x2, x3)
  }

  /** Tupling for functions of arity 4. This transforms a function
   *  of arity 4 into a unary function that takes a 4-tuple of arguments.
   */
  // @deprecated("use `f.tupled` instead")
  def tupled[T1, T2, T3, T4, R](f: (T1, T2, T3, T4) => R): ((T1, T2, T3, T4)) => R = {
    case ((x1, x2, x3, x4)) => f(x1, x2, x3, x4)
  }

  /** Tupling for functions of arity 5. This transforms a function
   *  of arity 5 into a unary function that takes a 5-tuple of arguments.
   */
  // @deprecated("use `f.tupled` instead")
  def tupled[T1, T2, T3, T4, T5, R](f: (T1, T2, T3, T4, T5) => R): ((T1, T2, T3, T4, T5)) => R = {
    case ((x1, x2, x3, x4, x5)) => f(x1, x2, x3, x4, x5)
  }

  /** Un-tupling for functions of arity 2. This transforms a function taking
   *  a pair of arguments into a binary function which takes each argument separately.
   *
   *  @tparam T1 the type of the 1st argument
   *  @tparam T2 the type of the 2nd argument
   *  @tparam R the result type
   *  @param f the tupled function to untuple
   *  @return an equivalent function of arity 2 that takes each argument separately
   */
  def untupled[T1, T2, R](f: ((T1, T2)) => R): (T1, T2) => R = {
    (x1, x2) => f((x1, x2))
  }

  /** Un-tupling for functions of arity 3. This transforms a function taking
   *  a triple of arguments into a ternary function which takes each argument separately.
   *
   *  @tparam T1 the type of the 1st argument
   *  @tparam T2 the type of the 2nd argument
   *  @tparam T3 the type of the 3rd argument
   *  @tparam R the result type
   *  @param f the tupled function to untuple
   *  @return an equivalent function of arity 3 that takes each argument separately
   */
  def untupled[T1, T2, T3, R](f: ((T1, T2, T3)) => R): (T1, T2, T3) => R = {
    (x1, x2, x3) => f((x1, x2, x3))
  }

  /** Un-tupling for functions of arity 4. This transforms a function taking
   *  a 4-tuple of arguments into a function of arity 4 which takes each argument separately.
   *
   *  @tparam T1 the type of the 1st argument
   *  @tparam T2 the type of the 2nd argument
   *  @tparam T3 the type of the 3rd argument
   *  @tparam T4 the type of the 4th argument
   *  @tparam R the result type
   *  @param f the tupled function to untuple
   *  @return an equivalent function of arity 4 that takes each argument separately
   */
  def untupled[T1, T2, T3, T4, R](f: ((T1, T2, T3, T4)) => R): (T1, T2, T3, T4) => R = {
    (x1, x2, x3, x4) => f((x1, x2, x3, x4))
  }

  /** Un-tupling for functions of arity 5. This transforms a function taking
   *  a 5-tuple of arguments into a function of arity 5 which takes each argument separately.
   *
   *  @tparam T1 the type of the 1st argument
   *  @tparam T2 the type of the 2nd argument
   *  @tparam T3 the type of the 3rd argument
   *  @tparam T4 the type of the 4th argument
   *  @tparam T5 the type of the 5th argument
   *  @tparam R the result type
   *  @param f the tupled function to untuple
   *  @return an equivalent function of arity 5 that takes each argument separately
   */
  def untupled[T1, T2, T3, T4, T5, R](f: ((T1, T2, T3, T4, T5)) => R): (T1, T2, T3, T4, T5) => R = {
    (x1, x2, x3, x4, x5) => f((x1, x2, x3, x4, x5))
  }

  // applyUsing ---------------------------------------------------------

  extension [R](f: () => R)
    /** Applies this function.
     *  @return the result of the function application
     */
    def applyUsing: R = f()

  extension [T1, R](f: T1 => R)
    /** Applies this function to the argument taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1): R = f(v1)

  extension [T1, T2, R](f: (T1, T2) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2): R = f(v1, v2)

  extension [T1, T2, T3, R](f: (T1, T2, T3) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3): R = f(v1, v2, v3)

  extension [T1, T2, T3, T4, R](f: (T1, T2, T3, T4) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4): R = f(v1, v2, v3, v4)

  extension [T1, T2, T3, T4, T5, R](f: (T1, T2, T3, T4, T5) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5): R = f(v1, v2, v3, v4, v5)

  extension [T1, T2, T3, T4, T5, T6, R](f: (T1, T2, T3, T4, T5, T6) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6): R = f(v1, v2, v3, v4, v5, v6)

  extension [T1, T2, T3, T4, T5, T6, T7, R](f: (T1, T2, T3, T4, T5, T6, T7) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7): R = f(v1, v2, v3, v4, v5, v6, v7)

  extension [T1, T2, T3, T4, T5, T6, T7, T8, R](f: (T1, T2, T3, T4, T5, T6, T7, T8) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8): R = f(v1, v2, v3, v4, v5, v6, v7, v8)

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9): R = f(v1, v2, v3, v4, v5, v6, v7, v8, v9)

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10): R = f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10)

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11): R = f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11)

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12): R = f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12)

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13): R = f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13)

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14): R = f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14)

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15): R = f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15)

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16): R = f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16)

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17): R = f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17)

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18): R = f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18)

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19): R = f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19)

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20): R = f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20)

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21): R = f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21)

  extension [T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22, R](f: (T1, T2, T3, T4, T5, T6, T7, T8, T9, T10, T11, T12, T13, T14, T15, T16, T17, T18, T19, T20, T21, T22) => R)
    /** Applies this function to the arguments taken from the context.
     *  @return the result of the function application
     */
    def applyUsing(using v1: T1, v2: T2, v3: T3, v4: T4, v5: T5, v6: T6, v7: T7, v8: T8, v9: T9, v10: T10, v11: T11, v12: T12, v13: T13, v14: T14, v15: T15, v16: T16, v17: T17, v18: T18, v19: T19, v20: T20, v21: T21, v22: T22): R = f(v1, v2, v3, v4, v5, v6, v7, v8, v9, v10, v11, v12, v13, v14, v15, v16, v17, v18, v19, v20, v21, v22)

  extension [F, Args <: Tuple, R](f: F)(using tf: scala.util.TupledFunction[F, Args => R])
    /** Applies this function of any arity to the arguments taken from the context.
     *  Functions of arity up to 22 use the non-experimental overloads above.
     *  @return the result of the function application
     */
    @experimental inline def applyUsing: R = tf.tupled(f)(compiletime.summonAll[Args])
}
