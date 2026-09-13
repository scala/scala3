package scala.scalajs.runtime

/* Before Scala.js 1.19, this class was concrete. It had a 1-argument
 * constructor taking a js.Function[Array[Object], Object], and its `apply()`
 * method called that function. This was similar to the `AnonFunctionN` classes
 * of the Scala.js library (shared between Scala 2 and 3).
 *
 * In Scala.js 1.19, we introduced `NewLambda` nodes, which superseded these
 * specialized classes with a compilation mode that is more efficient on Wasm.
 * However, libraries compiled with earlier versions still contain references
 * to `AnonFunctionXXL`.
 *
 * The IR deserializer patches allocations of the form
 *   New(AnonFunctionXXL, ctor, closure :: Nil)
 * into
 *   NewLambda(AnonFunctionXXL, ..., (xs: Array[Object]) => closure(xs))
 *
 * When the `closure` is directly a JS `Closure` with the right signature
 * (which is supposed to be always, as far as our codegens were concerned),
 * it rewrites that as
 *   NewLambda(AnonFunctionXXL, ..., (closureParam: Array[Object]) => closureBody)
 * which provides the best performance for old code.
 */
/** The runtime representation of anonymous functions of arity above 22 in code
 *  compiled by Scala.js before version 1.19, backed by a function that takes
 *  the arguments as a single array.
 *
 *  Retained only so that binaries compiled against earlier versions keep
 *  linking; the IR deserializer rewrites allocations of this class into
 *  `NewLambda` nodes.
 */
@deprecated("used by the codegen before Scala.js 1.19", since = "3.7.0")
sealed abstract class AnonFunctionXXL extends scala.runtime.FunctionXXL
