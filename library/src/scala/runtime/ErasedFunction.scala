package scala.runtime

import scala.annotation.experimental

/** Marker trait for function types with erased parameters.
 *
 *  This trait will be refined with an `apply` method with erased parameters:
 *      ErasedFunction { def apply([erased] x_1: P_1, ..., [erased] x_N: P_N): R }
 *  This type will be erased to FunctionL, where L = N - count(erased).
 */
@experimental trait ErasedFunction
