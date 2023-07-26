package scala.runtime

import scala.annotation.experimental

/** Marker trait for function types with erased parameters.
 *
 *  This trait will be refined with an `apply` method with erased parameters:
 *      ErasedFunction { def apply([erased] x_1: P_1, ..., [erased] x_N: P_N): R }
 *  This type will be erased to FunctionL, where L = N - count(erased).
 *
 *  Note: Now we use `scala.PolyFunction` instead. This will be removed.
 */
@experimental trait ErasedFunction // TODO delete. Cannot be deleted until the reference compiler stops using it.
