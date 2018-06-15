package scala

/** Marker trait for polymorphic function types.
 *
 *  This is the only trait that can be refined with a polymorphic method,
 *  as long as that method is called `apply`, e.g.:
 *      PolyFunction { def apply[T_1, ..., T_M](x_1: P_1, ..., x_N: P_N): R }
 *  This type will be erased to FunctionN.
 */
trait PolyFunction
