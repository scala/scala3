package dotty.tools
package dotc
package interactive

import scala.annotation.tailrec
import scala.collection._

import ast.{NavigateAST, Trees, tpd, untpd}
import core._, core.Decorators._
import Contexts._, Flags._, Names._, NameOps._, Symbols._, Trees._, Types._
import transform.SymUtils.decorateSymbol
import util.Spans._, util.SourceFile, util.SourcePosition
import core.Denotations.SingleDenotation
import NameKinds.SimpleNameKind
import config.Printers.interactiv
import StdNames.nme

/** High-level API to get information out of typed trees, designed to be used by IDEs.
 *
 *  @see `InteractiveDriver` to get typed trees from code.
 */
object Interactive {
  import ast.tpd._

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
    tree.isInstanceOf[DefTree with NameTree]

  /** The type of the closest enclosing tree with a type containing position `pos`. */
  def enclosingType(trees: List[SourceTree], pos: SourcePosition)(implicit ctx: Context): Type = {
    val path = pathTo(trees, pos)
    if (path.isEmpty) NoType
    else path.head.tpe
  }

  /** The closest enclosing tree with a symbol containing position `pos`, or the `EmptyTree`.
   */
  def enclosingTree(trees: List[SourceTree], pos: SourcePosition)(implicit ctx: Context): Tree =
    enclosingTree(pathTo(trees, pos))

  /** The closes enclosing tree with a symbol, or the `EmptyTree`.
   */
  def enclosingTree(path: List[Tree])(implicit ctx: Context): Tree =
    path.dropWhile(!_.symbol.exists).headOption.getOrElse(tpd.EmptyTree)

  /**
   * The source symbols that are the closest to `path`.
   *
   * If this path ends in an import, then this returns all the symbols that are imported by this
   * import statement.
   *
   * @param path The path to the tree whose symbols to extract.
   * @return The source symbols that are the closest to `path`.
   *
   * @see sourceSymbol
   */
  def enclosingSourceSymbols(path: List[Tree], pos: SourcePosition)(implicit ctx: Context): List[Symbol] = {
    val syms = path match {
      // For a named arg, find the target `DefDef` and jump to the param
      case NamedArg(name, _) :: Apply(fn, _) :: _ =>
        val funSym = fn.symbol
        if (funSym.name == StdNames.nme.copy
          && funSym.is(Synthetic)
          && funSym.owner.is(CaseClass)) {
            List(funSym.owner.info.member(name).symbol)
        } else {
          val classTree = funSym.topLevelClass.asClass.rootTree
          val paramSymbol =
            for {
              DefDef(_, _, paramss, _, _) <- tpd.defPath(funSym, classTree).lastOption
              param <- paramss.flatten.find(_.name == name)
            } yield param.symbol
          List(paramSymbol.getOrElse(fn.symbol))
        }

      // For constructor calls, return the `<init>` that was selected
      case _ :: (_:  New) :: (select: Select) :: _ =>
        List(select.symbol)

      case (_: Thicket) :: (imp: Import) :: _ =>
        importedSymbols(imp, _.span.contains(pos.span))

      case (imp: Import) :: _ =>
        importedSymbols(imp, _.span.contains(pos.span))

      case _ =>
        List(enclosingTree(path).symbol)
    }

    syms.map(_.sourceSymbol).filter(_.exists)
  }

  /** Check if `tree` matches `sym`.
   *  This is the case if the symbol defined by `tree` equals `sym`,
   *  or the source symbol of tree equals sym,
   *  or `include` is `overridden`, and `tree` is overridden by `sym`,
   *  or `include` is `overriding`, and `tree` overrides `sym`.
   */
  def matchSymbol(tree: Tree, sym: Symbol, include: Include.Set)(implicit ctx: Context): Boolean = {

    def overrides(sym1: Symbol, sym2: Symbol) =
      sym1.owner.derivesFrom(sym2.owner) && sym1.overriddenSymbol(sym2.owner.asClass) == sym2

    (  sym == tree.symbol
    || sym.exists && sym == tree.symbol.sourceSymbol
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
   (implicit ctx: Context): List[SourceTree] =
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
                )(implicit ctx: Context): List[SourceTree] = safely {
    val buf = new mutable.ListBuffer[SourceTree]

    def traverser(source: SourceFile) = {
      new untpd.TreeTraverser {
        private def handle(utree: untpd.NameTree): Unit = {
          val tree = utree.asInstanceOf[tpd.NameTree]
          if (tree.symbol.exists
               && !tree.symbol.is(Synthetic)
               && !tree.symbol.isPrimaryConstructor
               && tree.span.exists
               && !tree.span.isZeroExtent
               && (include.isReferences || isDefinition(tree))
               && treePredicate(tree))
            buf += SourceTree(tree, source)
        }
        override def traverse(tree: untpd.Tree)(implicit ctx: Context) = {
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
                       )(implicit ctx: Context): List[SourceTree] = {
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
   */
  def pathTo(trees: List[SourceTree], pos: SourcePosition)(implicit ctx: Context): List[Tree] =
    trees.find(_.pos.contains(pos)) match {
      case Some(tree) => pathTo(tree.tree, pos.span)
      case None => Nil
    }

  def pathTo(tree: Tree, span: Span)(implicit ctx: Context): List[Tree] =
    if (tree.span.contains(span))
      NavigateAST.pathTo(span, tree, skipZeroExtent = true)
        .collect { case t: untpd.Tree => t }
        .dropWhile(!_.hasType).asInstanceOf[List[tpd.Tree]]
    else Nil

  def contextOfStat(stats: List[Tree], stat: Tree, exprOwner: Symbol, ctx: Context): Context = stats match {
    case Nil =>
      ctx
    case first :: _ if first eq stat =>
      ctx.exprContext(stat, exprOwner)
    case (imp: Import) :: rest =>
      contextOfStat(rest, stat, exprOwner, ctx.importContext(imp, imp.symbol(ctx)))
    case _ :: rest =>
      contextOfStat(rest, stat, exprOwner, ctx)
  }

  def contextOfPath(path: List[Tree])(implicit ctx: Context): Context = path match {
    case Nil | _ :: Nil =>
      ctx.run.runContext.fresh.setCompilationUnit(ctx.compilationUnit)
    case nested :: encl :: rest =>
      val outer = contextOfPath(encl :: rest)
      try encl match {
        case tree @ PackageDef(pkg, stats) =>
          assert(tree.symbol.exists)
          if (nested `eq` pkg) outer
          else contextOfStat(stats, nested, pkg.symbol.moduleClass, outer.packageContext(tree, tree.symbol))
        case tree: DefDef =>
          assert(tree.symbol.exists)
          val localCtx = outer.localContext(tree, tree.symbol).setNewScope
          for (tparam <- tree.tparams) localCtx.enter(tparam.symbol)
          for (vparams <- tree.vparamss; vparam <- vparams) localCtx.enter(vparam.symbol)
            // Note: this overapproximates visibility a bit, since value parameters are only visible
            // in subsequent parameter sections
          localCtx
        case tree: MemberDef =>
          assert(tree.symbol.exists)
          outer.localContext(tree, tree.symbol)
        case tree @ Block(stats, expr) =>
          val localCtx = outer.fresh.setNewScope
          stats.foreach {
            case stat: MemberDef => localCtx.enter(stat.symbol)
            case _ =>
          }
          contextOfStat(stats, nested, ctx.owner, localCtx)
        case tree @ CaseDef(pat, guard, rhs) if nested `eq` rhs =>
          val localCtx = outer.fresh.setNewScope
          pat.foreachSubTree {
            case bind: Bind => localCtx.enter(bind.symbol)
            case _ =>
          }
          localCtx
        case tree @ Template(constr, parents, self, _) =>
          if ((constr :: self :: parents).contains(nested)) ctx
          else contextOfStat(tree.body, nested, tree.symbol, outer.inClassContext(self.symbol))
        case _ =>
          outer
      }
      catch {
        case ex: CyclicReference => outer
      }
  }

  /** The first tree in the path that is a definition. */
  def enclosingDefinitionInPath(path: List[Tree])(implicit ctx: Context): Tree =
    path.find(_.isInstanceOf[DefTree]).getOrElse(EmptyTree)

  /**
   * Find the definitions of the symbol at the end of `path`. In the case of an import node,
   * all imported symbols will be considered.
   *
   * @param path   The path to the symbol for which we want the definitions.
   * @param driver The driver responsible for `path`.
   * @return The definitions for the symbol at the end of `path`.
   */
  def findDefinitions(path: List[Tree], pos: SourcePosition, driver: InteractiveDriver): List[SourceTree] = {
    implicit val ctx = driver.currentCtx
    val enclTree = enclosingTree(path)
    val includeOverridden = enclTree.isInstanceOf[MemberDef]
    val symbols = enclosingSourceSymbols(path, pos)
    val includeExternal = symbols.exists(!_.isLocal)
    findDefinitions(symbols, driver, includeOverridden, includeExternal)
  }

  /**
   * Find the definitions of `symbols`.
   *
   * @param symbols           The list of symbols for which to find a definition.
   * @param driver            The driver responsible for the given symbols.
   * @param includeOverridden If true, also include the symbols overridden by any of `symbols`.
   * @param includeExternal   If true, also look for definitions on the classpath.
   * @return The definitions for the symbols in `symbols`, and if `includeOverridden` is set, the
   *         definitions for the symbols that they override.
   */
  def findDefinitions(symbols: List[Symbol],
                      driver: InteractiveDriver,
                      includeOverridden: Boolean,
                      includeExternal: Boolean): List[SourceTree] = {
    implicit val ctx = driver.currentCtx
    val include = Include.definitions | Include.overriding |
      (if (includeOverridden) Include.overridden else Include.empty)
    symbols.flatMap { sym =>
      val name = sym.name.sourceModuleName.toString
      val includeLocal = if (sym.exists && sym.isLocal) Include.local else Include.empty
      val trees =
        if (includeExternal) driver.allTreesContaining(name)
        else driver.sourceTreesContaining(name)
      findTreesMatching(trees, include | includeLocal, sym)
    }
  }

  /**
   * Given `sym`, originating from `sourceDriver`, find its representation in
   * `targetDriver`.
   *
   * @param symbol The symbol to expression in the new driver.
   * @param sourceDriver The driver from which `symbol` originates.
   * @param targetDriver The driver in which we want to get a representation of `symbol`.
   * @return A representation of `symbol` in `targetDriver`.
   */
  def localize(symbol: Symbol, sourceDriver: InteractiveDriver, targetDriver: InteractiveDriver): Symbol = {

    def in[T](driver: InteractiveDriver)(fn: Context => T): T =
      fn(driver.currentCtx)

    if (sourceDriver == targetDriver) symbol
    else {
      val owners = in(sourceDriver) { implicit ctx =>
        symbol.ownersIterator.toList.reverse.map(_.name)
      }
      in(targetDriver) { implicit ctx =>
        val base: Symbol = ctx.definitions.RootClass
        owners.tail.foldLeft(base) { (prefix, symbolName) =>
          if (prefix.exists) prefix.info.member(symbolName).symbol
          else NoSymbol
        }
      }
    }
  }

  /**
   * Return a predicate function that determines whether a given `NameTree` is an implementation of
   * `sym`.
   *
   * @param sym The symbol whose implementations to find.
   * @return A function that determines whether a `NameTree` is an implementation of `sym`.
   */
  def implementationFilter(sym: Symbol)(implicit ctx: Context): NameTree => Boolean = {
    if (sym.isClass) {
      case td: TypeDef =>
        val treeSym = td.symbol
        (treeSym != sym || !treeSym.isOneOf(AbstractOrTrait)) && treeSym.derivesFrom(sym)
      case _ =>
        false
    } else {
      case md: MemberDef =>
        matchSymbol(md, sym, Include.overriding) && !md.symbol.is(Deferred)
      case _ =>
        false
    }
  }

  /**
   * Is this tree using a renaming introduced by an import statement or an alias for `this`?
   *
   * @param tree The tree to inspect
   * @return True, if this tree's name is different than its symbol's name, indicating that
   *         it uses a renaming introduced by an import statement or an alias for `this`.
   */
  def isRenamed(tree: NameTree)(implicit ctx: Context): Boolean = {
    val symbol = tree.symbol
    symbol.exists && !sameName(tree.name, symbol.name)
  }

  /** Are the two names the same? */
  def sameName(n0: Name, n1: Name): Boolean = {
    n0.stripModuleClassSuffix.toTermName eq n1.stripModuleClassSuffix.toTermName
  }

  private[interactive] def safely[T](op: => List[T]): List[T] =
    try op catch { case ex: TypeError => Nil }

}
