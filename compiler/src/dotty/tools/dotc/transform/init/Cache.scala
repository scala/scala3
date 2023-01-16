package dotty.tools.dotc
package transform
package init

import core.*
import Contexts.*

import ast.tpd
import tpd.Tree

class Cache[Config, Res]:
  import Cache.*

  /** The cache for expression values from last iteration */
  protected var last: ExprValueCache[Config, Res] =  Map.empty

  /** The output cache for expression values
   *
   *  The output cache is computed based on the cache values `last` from the
   *  last iteration.
   *
   *  Both `last` and `current` are required to make sure an encountered
   *  expression is evaluated once in each iteration.
   */
  protected var current: ExprValueCache[Config, Res] = Map.empty

  /** Whether the current heap is different from the last heap?
   *
   *  `changed == false` implies that the fixed point has been reached.
   */
  protected var changed: Boolean = false

  /** Used to avoid allocation, its state does not matter */
  protected given MutableTreeWrapper = new MutableTreeWrapper

  def get(config: Config, expr: Tree): Option[Res] =
    current.get(config, expr)

  /** Copy the value (config, expr)` from the last cache to the current cache
   *
   *  It assumes `default` if it doesn't exist in the last cache.
   *
   *  It updates the current caches if the values change.
   *
   *  The two caches are required because we want to make sure in a new iteration, an expression is evaluated once.
   */
  def cachedEval(config: Config, expr: Tree, cacheResult: Boolean, default: Res)(eval: => Res): Res =
    this.get(config, expr) match
    case Some(value) => value
    case None =>
      val assumeValue: Res =
        last.get(config, expr) match
        case Some(value) => value
        case None =>
          this.last = last.updatedNested(config, expr, default)
          default

      this.current = current.updatedNested(config, expr, assumeValue)

      val actual = eval
      if actual != assumeValue then
        // println("Changed! from = " + assumeValue + ", to = " + actual)
        this.changed = true
        // TODO: respect cacheResult to reduce cache size
        this.current = this.current.updatedNested(config, expr, actual)
        // this.current = this.current.removed(config, expr)
      end if

      actual
  end cachedEval

  def hasChanged = changed

  /** Prepare cache for the next iteration
   *
   *  1. Reset changed flag.
   *
   *  2. Use current cache as last cache and set current cache to be empty.
   */
  def prepareForNextIteration()(using Context) =
    this.changed = false
    this.last = this.current
    this.current = Map.empty
end Cache

object Cache:
  type ExprValueCache[Config, Res] = Map[Config, Map[TreeWrapper, Res]]

  /** A wrapper for trees for storage in maps based on referential equality of trees. */
  abstract class TreeWrapper:
    def tree: Tree

    override final def equals(other: Any): Boolean =
      other match
      case that: TreeWrapper => this.tree eq that.tree
      case _ => false

    override final def hashCode = tree.hashCode

  /** The immutable wrapper is intended to be stored as key in the heap. */
  class ImmutableTreeWrapper(val tree: Tree) extends TreeWrapper

  /** For queries on the heap, reuse the same wrapper to avoid unnecessary allocation.
   *
   *  A `MutableTreeWrapper` is only ever used temporarily for querying a map,
   *  and is never inserted to the map.
   */
  class MutableTreeWrapper extends TreeWrapper:
    var queryTree: Tree | Null = null
    def tree: Tree = queryTree match
      case tree: Tree => tree
      case null => ???

  extension [Config, Res](cache: ExprValueCache[Config, Res])
    def get(config: Config, expr: Tree)(using queryWrapper: MutableTreeWrapper): Option[Res] =
      queryWrapper.queryTree = expr
      cache.get(config).flatMap(_.get(queryWrapper))

    def removed(config: Config, expr: Tree)(using queryWrapper: MutableTreeWrapper) =
      queryWrapper.queryTree = expr
      val innerMap2 = cache(config).removed(queryWrapper)
      cache.updated(config, innerMap2)

    def updatedNested(config: Config, expr: Tree, result: Res): ExprValueCache[Config, Res] =
      val wrapper = new ImmutableTreeWrapper(expr)
      updatedNestedWrapper(config, wrapper, result)

    def updatedNestedWrapper(config: Config, wrapper: ImmutableTreeWrapper, result: Res): ExprValueCache[Config, Res] =
      val innerMap = cache.getOrElse(config, Map.empty[TreeWrapper, Res])
      val innerMap2 = innerMap.updated(wrapper, result)
      cache.updated(config, innerMap2)
  end extension
