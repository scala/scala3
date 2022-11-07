// TODO in which package should this class be located?
package scala
package annotation

import scala.quoted._

/** Base trait for macro annotation that will transform a definition */
@experimental
trait MacroAnnotation extends StaticAnnotation:

  /** Transform the `tree` definition and add other definitions
   *
   *  This method takes as argument the annotated definition.
   *  It returns a non-empty list containing the modified version of the annotated definition.
   *  The new tree for the definition must use the original symbol.
   *  New definitions can be added to the list before or after the transformed definitions, this order
   *  will be retained.
   *
   *  All definitions in the result must have the same owner. The owner can be recovered from `tree.symbol.owner`.
   *
   *  The result cannot contain `class`, `object` or `type` definition. This limitation will be relaxed in the future.
   *
   *  When developing and testing a macro annotation, you must enable `-Xcheck-macros` and `-Ycheck:all`.
   *
   *  Example:
   *  ```scala
   *  import scala.quoted.*
   *  import scala.collection.mutable
   *
   *  class memoize extends MacroAnnotation:
   *    def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition] =
   *      import quotes.reflect._
   *      tree match
   *        case DefDef(name, TermParamClause(param :: Nil) :: Nil, tpt, Some(rhsTree)) =>
   *          (Ref(param.symbol).asExpr, rhsTree.asExpr) match
   *            case ('{ $paramRefExpr: t }, '{ $rhsExpr: u }) =>
   *              val cacheSymbol = Symbol.newUniqueVal(tree.symbol.owner, name + "Cache", TypeRepr.of[mutable.Map[t, u]], Flags.Private, Symbol.noSymbol)
   *              val cacheRhs = '{ mutable.Map.empty[t, u] }.asTerm
   *              val cacheVal = ValDef(cacheSymbol, Some(cacheRhs))
   *              val cacheRefExpr = Ref(cacheSymbol).asExprOf[mutable.Map[t, u]]
   *              val newRhs = '{ $cacheRefExpr.getOrElseUpdate($paramRefExpr, $rhsExpr) }.asTerm
   *              val newTree = DefDef.copy(tree)(name, TermParamClause(param :: Nil) :: Nil, tpt, Some(newRhs))
   *              List(cacheVal, newTree)
   *        case _ =>
   *          report.error("Annotation only supported on `def` with a single argument are supported")
   *          List(tree)
   *  ```
   *  with this macro annotation a user can write
   *  ```scala sc:nocompile
   *  @memoize
   *  def fib(n: Int): Int =
   *    println(s"compute fib of $n")
   *    if n <= 1 then n else fib(n - 1) + fib(n - 2)
   *  ```
   *  and the macro will modify the definition to create
   *  ```scala
   *   val fibCache =
   *     scala.collection.mutable.Map.empty[Int, Int]
   *   def fib(n: Int): Int =
   *     fibCache.getOrElseUpdate(
   *       n,
   *       {
   *         println(s"compute fib of $n")
   *         if n <= 1 then n else fib(n - 1) + fib(n - 2)
   *       }
   *     )
   *  ```
   *
   *  @param Quotes Implicit instance of Quotes used for tree reflection
   *  @param tree   Tree that will be transformed
   */
  def transform(using Quotes)(tree: quotes.reflect.Definition): List[quotes.reflect.Definition]
