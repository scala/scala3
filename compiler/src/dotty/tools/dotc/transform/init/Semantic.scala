package dotty.tools.dotc
package transform
package init

import core._
import Symbols._

import ast.tpd._
import util.SourcePosition

import scala.collection.mutable

trait Semantic { self: Domain =>
  /** Cache used to terminate the analysis
   *
   * A finitary configuration is not enough for the analysis to
   * terminate.  We need to use cache to let the interpreter "know"
   * that it can terminate.
   */
  type Cache <: mutable.Map[Config, Value]
  val cache: Cache

  /** Errors in the program */
  type Error

  /** Result of abstract interpretation */
  case class Result(value: Value, heap: Heap, errors: List[Error])

  /** Evaluate an expression with the given value for `this` in a given class `klass`
   *
   * This method only handles cache logic and delegates the work to `cases`.
   */
  def eval(expr: Tree, thisV: Value, klass: ClassSymbol, heap: Heap): Result =
    val cfg = makeConfig(expr, thisV, heap)
    if (cache.contains(cfg)) Result(cache(cfg), heap, Nil)
    else {
      // no need to compute fix-point, because
      // 1. the result is decided by `cfg` for a legal program
      //    (heap change is irrelevant thanks to monotonicity)
      // 2. errors will have been reported for an illegal program
      cache(cfg) = bottom
      val res = cases(expr, thisV, klass, heap)
      cache(cfg) = res.value
      res
    }

  /** Handles the evaluation of different expressions */
  def cases(expr: Tree, thisV: Value, klass: ClassSymbol, heap: Heap): Result = ???

  /** Initialize an abstract object */
  def init(klass: Symbol, thisV: Addr, heap: Heap): Result = ???
}
