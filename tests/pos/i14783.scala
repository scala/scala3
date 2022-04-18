object Wart:
  def bar(using c: Ctx)(ws: List[Wrap[c.type]]): Unit =
    ws.zipWithIndex.foreach { (w, _) => w.x.foo }

trait Wrap[C <: Ctx & Singleton]:
  val ctx: C
  def x: ctx.inner.X

trait Ctx:
  object inner:
    type X
    extension (self: X) def foo: Int = ???


object WartInspector:
  def myWartTraverser: WartTraverser = ???
  def inspect(using q: Quotes)(tastys: List[Tasty[q.type]]): Unit = {
    val universe: WartUniverse.Aux[q.type] = WartUniverse(q)
    val traverser = myWartTraverser.get(universe)
    tastys.zipWithIndex.foreach { (tasty, index) =>
      val tree = tasty.ast
      traverser.traverseTree(tree)(tree.symbol)
    }
  }

object WartUniverse:
  type Aux[X <: Quotes] = WartUniverse { type Q = X }
  def apply[Q <: Quotes](quotes: Q): Aux[Q] = ???


abstract class WartUniverse:
  type Q <: Quotes
  val quotes: Q
  abstract class Traverser extends quotes.reflect.TreeTraverser


abstract class WartTraverser:
  def get(u: WartUniverse): u.Traverser

trait Tasty[Q <: Quotes & Singleton]:
  val quotes: Q
  def path: String
  def ast: quotes.reflect.Tree

trait Quotes:
  object reflect:
    type Tree
    extension (self: Tree) def symbol: Symbol = ???
    type Symbol
    trait TreeTraverser:
      def traverseTree(tree: Tree)(symbol: Symbol): Unit