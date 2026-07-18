package scala

import language.experimental.captureChecking

/* (EXPERIMENTAL) NEW DOCUMENTATION: This should be used when we stabilize erased definitions or other generalize other function types.
 *
 * Marker trait for many (poly) function types.
 *
 *  This trait can only be refined with a method or polymorphic method,
 *  as long as that method is called `apply`, e.g.:
 *      PolyFunction { def apply(x_1: P_1, ..., x_N: P_N): R }
 *      PolyFunction { def apply[T_1, ..., T_M](x_1: P_1, ..., x_N: P_N): R }
 *  Exactly one term argument list is expected.
 *  The term argument list may be contextual and each argument may be erased.
 *
 *  This type will be erased to FunctionN or FunctionXXL.
 */
/** Marker trait for polymorphic function types.
 *
 *  This trait can only be refined with a polymorphic method,
 *  as long as that method is called `apply`, e.g.:
 *      PolyFunction { def apply[T_1, ..., T_M](x_1: P_1, ..., x_N: P_N): R }
 *  Exactly one term argument list is expected.
 *  The term argument list may be contextual.
 *
 *  This type will be erased to FunctionN or FunctionXXL.
 */
trait PolyFunction
