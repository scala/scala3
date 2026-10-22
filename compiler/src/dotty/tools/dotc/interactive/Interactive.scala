package dotty.tools
package dotc
package interactive

import scala.collection.*

import ast.{NavigateAST, Trees, tpd, untpd}
import core.*
import Decorators.*, ContextOps.*
import Contexts.*, Flags.*, Names.*, NameOps.*, Symbols.*, Trees.*, Types.*
import util.Spans.*, util.SourceFile, util.SourcePosition

/** High-level API to get information out of typed trees, designed to be used by IDEs.
 *
 *  @see `InteractiveDriver` to get typed trees from code.
 */
object Interactive {
  import ast.tpd.*

  object Include {
    case class Set private[Include] (val bits: Int) extends AnyVal {
      def | (that: Set): Set = Set(bits | that.bits)
      def except(that: Set): Set = Set(bits & ~that.bits)

      def isEmpty: Boolean = bits == 0
      def isOverridden: Boolean = (bits & overridden.bits) != 0
      def isOverriding: Boolean = (bits & overriding.bits) != 0
      def isReferences: Boolean = (bits & references.bits) != 0
      def isDefinitions: Boolean = (bits & definitions.bits) != 0
      def isLinkedClass: Boolean = (bits & linkedClass.bits) != 0
      def isImports: Boolean = (bits & imports.bits) != 0
      def isLocal: Boolean = (bits & local.bits) != 0
    }

    /** The empty set */
    val empty: Set = Set(0)

    /** Include trees whose symbol is overridden by `sym` */
    val overridden: Set = Set(1 << 0)

    /** Include trees whose symbol overrides `sym` */
    val overriding: Set = Set(1 << 1)

    /** Include references */
    val references: Set = Set(1 << 2)

    /** Include definitions */
    val definitions: Set = Set(1 << 3)

    /** Include `sym.linkedClass */
    val linkedClass: Set = Set(1 << 4)

    /** Include imports in the results */
    val imports: Set = Set(1 << 5)

    /** Include local symbols, inspect local trees */
    val local: Set = Set(1 << 6)

    /** All the flags */
    val all: Set = Set(~0)
  }

  /** Does this tree define a symbol ? */
  def isDefinition(tree: Tree): Boolean =
    tree.isInstanceOf[NamedDefTree]

  /** Check if `tree` matches `sym`.
   *  This is the case if the symbol defined by `tree` equals `sym`,
   *  or the source symbol of tree equals sym,
   *  or `include` is `overridden`, and `tree` is overridden by `sym`,
   *  or `include` is `overriding`, and `tree` overrides `sym`.
   */
  def matchSymbol(tree: Tree, sym: Symbol, include: Include.Set)(using Context): Boolean = {

    def overrides(sym1: Symbol, sym2: Symbol) =
      sym1.owner.derivesFrom(sym2.owner) && sym1.overriddenSymbol(sym2.owner.asClass) == sym2

    (  sym == tree.symbol
    || sym.exists && sym == tree.symbol.sourceSymbol
    || sym.exists && sym.sourceSymbol == tree.symbol
    || !include.isEmpty && sym.name == tree.symbol.name && sym.maybeOwner != tree.symbol.maybeOwner
       && (  include.isOverridden && overrides(sym, tree.symbol)
          || include.isOverriding && overrides(tree.symbol, sym)
          )
    )
  }

  /** Find named trees with a non-empty position whose symbol match `sym` in `trees`.
   *
   *  Note that nothing will be found for symbols not defined in source code,
   *  use `sourceSymbol` to get a symbol related to `sym` that is defined in
   *  source code.
   */
  def namedTrees(trees: List[SourceTree], include: Include.Set, sym: Symbol)
   (using Context): List[SourceTree] =
    if (!sym.exists)
      Nil
    else
      namedTrees(trees, include, matchSymbol(_, sym, include))

  /** Find named trees with a non-empty position satisfying `treePredicate` in `trees`.
   *
   *  @param trees         The trees to inspect.
   *  @param include       Whether to include references, definitions, etc.
   *  @param treePredicate An additional predicate that the trees must match.
   *  @return The trees with a non-empty position satisfying `treePredicate`.
   */
  def namedTrees(trees: List[SourceTree],
                 include: Include.Set,
                 treePredicate: NameTree => Boolean = util.common.alwaysTrue
                )(using Context): List[SourceTree] = safely {
    val buf = new mutable.ListBuffer[SourceTree]

    def traverser(source: SourceFile) =
      new untpd.TreeTraverser {
        private def handle(utree: untpd.NameTree): Unit = {
          val tree = utree.asInstanceOf[tpd.NameTree]
          if (tree.symbol.exists
               && tree.name != StdNames.nme.ERROR
               && !tree.symbol.is(Synthetic)
               && !tree.symbol.isPrimaryConstructor
               && tree.span.exists
               && !tree.span.isZeroExtent
               && (include.isReferences || isDefinition(tree))
               && treePredicate(tree))
            buf += SourceTree(tree, source)
        }
        override def traverse(tree: untpd.Tree)(using Context) =
          tree match {
            case imp: untpd.Import if include.isImports && tree.hasType =>
              val tree = imp.asInstanceOf[tpd.Import]
              val selections = tpd.importSelections(tree)
              traverse(imp.expr)
              selections.foreach(traverse)
            case utree: untpd.ValOrDefDef if tree.hasType =>
              handle(utree)
              if (include.isLocal) traverseChildren(tree)
            case utree: untpd.NameTree if tree.hasType =>
              handle(utree)
              traverseChildren(tree)
            case tree: untpd.Inlined =>
              traverse(tree.call)
            case _ =>
              traverseChildren(tree)
          }
      }

    trees.foreach(t => traverser(t.source).traverse(t.tree))

    buf.toList
  }

  /**
   * Find trees that match `symbol` in `trees`.
   *
   * @param trees     The trees to inspect.
   * @param includes  Whether to include references, definitions, etc.
   * @param symbol    The symbol for which we want to find references.
   * @param predicate An additional predicate that the trees must match.
   */
  def findTreesMatching(trees: List[SourceTree],
                        includes: Include.Set,
                        symbol: Symbol,
                        predicate: NameTree => Boolean = util.common.alwaysTrue
                       )(using Context): List[SourceTree] = {
    val linkedSym = symbol.linkedClass
    val fullPredicate: NameTree => Boolean = tree =>
      (  (includes.isDefinitions || !Interactive.isDefinition(tree))
      && (  Interactive.matchSymbol(tree, symbol, includes)
         || ( includes.isLinkedClass
            && linkedSym.exists
            && Interactive.matchSymbol(tree, linkedSym, includes)
            )
         )
      && predicate(tree)
      )
    namedTrees(trees, includes, fullPredicate)
  }

  /** The reverse path to the node that closest encloses position `pos`,
   *  or `Nil` if no such path exists. If a non-empty path is returned it starts with
   *  the tree closest enclosing `pos` and ends with an element of `trees`.
   *
   *  Note that if the given `pos` points out places for incomplete parses,
   *  this method returns `errorTermTree` (`Literal(Consotant(null)`).
   *
   *  @see https://github.com/scala/scala3/issues/15294
   */
  def pathTo(trees: List[SourceTree], pos: SourcePosition)(using Context): List[Tree] =
    pathTo(trees.map(_.tree), pos.span)

  def pathTo(tree: Tree, span: Span)(using Context): List[Tree] =
    pathTo(List(tree), span)

  private def pathTo(trees: List[Tree], span: Span)(using Context): List[Tree] =
    if (trees.exists(_.span.contains(span)))
      NavigateAST.pathTo(span, trees, skipZeroExtent = true)
        .collect { case t: untpd.Tree => t }
        .dropWhile(!_.hasType).asInstanceOf[List[tpd.Tree]]
    else Nil

  def contextOfStat(stats: List[Tree], stat: Tree, exprOwner: Symbol, ctx: Context): Context = stats match {
    case Nil =>
      ctx
    case first :: _ if first eq stat =>
      ctx.exprContext(stat, exprOwner)
    case (imp: Import) :: rest =>
      contextOfStat(rest, stat, exprOwner, ctx.importContext(imp, inContext(ctx){imp.symbol}))
    case _ :: rest =>
      contextOfStat(rest, stat, exprOwner, ctx)
  }

  def contextOfPath(path: List[Tree])(using Context): Context = path match {
    case Nil | _ :: Nil =>
      ctx.fresh
    case nested :: encl :: rest =>
      val outer = contextOfPath(encl :: rest)
      try encl match {
        case tree @ PackageDef(pkg, stats) if tree.symbol.exists =>
          if (nested `eq` pkg) outer
          else contextOfStat(stats, nested, pkg.symbol.moduleClass, outer.packageContext(tree, tree.symbol))
        case tree: DefDef if tree.symbol.exists =>
          val localCtx = outer.localContext(tree, tree.symbol).setNewScope
          for params <- tree.paramss; param <- params do localCtx.enter(param.symbol)
            // Note: this overapproximates visibility a bit, since value parameters are only visible
            // in subsequent parameter sections
          localCtx
        case tree: MemberDef =>
          if (tree.symbol.exists)
            outer.localContext(tree, tree.symbol)
          else
            outer
        case tree @ Block(stats, expr) =>
          val localCtx = outer.localContext(tree, outer.owner).setNewScope
          stats.foreach {
            case stat: MemberDef => localCtx.enter(stat.symbol)
            case _ =>
          }
          contextOfStat(stats, nested, localCtx.owner, localCtx)
        case tree @ CaseDef(pat, _, _) =>
          val localCtx = outer.localContext(tree, outer.owner).setNewScope
          pat.foreachSubTree {
            case bind: Bind => localCtx.enter(bind.symbol)
            case _ =>
          }
          localCtx
        case tree @ Template(constr, _, self, _) =>
          if ((constr :: self :: tree.parentsOrDerived).contains(nested)) outer
          else contextOfStat(tree.body, nested, tree.symbol, outer.inClassContext(self.symbol))
        case _ =>
          outer
      }
      catch {
        case ex: CyclicReference => outer
      }
  }

  /** Some information about the trees is lost after Typer such as Extension method construct
   *  is expanded into methods. In order to support completions in those cases
   *  we have to rely on untyped trees and only when types are necessary use typed trees.
   */
  def resolveTypedOrUntypedPath(tpdPath: List[Tree], pos: SourcePosition)(using Context): List[untpd.Tree] =
    lazy val untpdPath: List[untpd.Tree] = NavigateAST
      .pathTo(pos.span, List(ctx.compilationUnit.untpdTree), true).collect:
        case untpdTree: untpd.Tree => untpdTree

    tpdPath match
      case (_: Bind) :: _ => tpdPath
      case (_: untpd.TypTree) :: _ => tpdPath
      case _ => untpdPath

  /** https://scala-lang.org/files/archive/spec/3.4/02-identifiers-names-and-scopes.html
   * import java.lang.*
   * {
   *   import scala.*
   *   {
   *     import Predef.*
   *     { /* source */ }
   *   }
   * }
   */
  def isImportedByDefault(sym: Symbol)(using Context): Boolean =
    val owner = sym.effectiveOwner
    owner == defn.ScalaPredefModuleClass || owner == defn.ScalaPackageClass || owner == defn.JavaLangPackageClass

  private[interactive] def safely[T](op: => List[T]): List[T] =
    try op catch { case ex: TypeError => Nil }
}

