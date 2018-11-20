package dotty.tools
package dotc
package interactive

import scala.annotation.tailrec
import scala.collection._

import ast.{NavigateAST, Trees, tpd, untpd}
import core._, core.Decorators.{sourcePos => _, _}
import Contexts._, Flags._, Names._, NameOps._, Symbols._, Trees._, Types._
import util.Positions._, util.SourceFile, util.SourcePosition
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
    }

    /** The empty set */
    val empty: Set = Set(0)

    /** Include trees whose symbol is overridden by `sym` */
    val overridden: Set = Set(1 << 0)

    /**
     * Include trees whose symbol overrides `sym` (but for performance only in same source
     * file)
     */
    val overriding: Set = Set(1 << 1)

    /** Include references */
    val references: Set = Set(1 << 2)

    /** Include definitions */
    val definitions: Set = Set(1 << 3)

    /** Include `sym.linkedClass */
    val linkedClass: Set = Set(1 << 4)

    /** Include imports in the results */
    val imports: Set = Set(1 << 5)

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
        importedSymbols(imp, _.pos.contains(pos.pos))

      case (imp: Import) :: _ =>
        importedSymbols(imp, _.pos.contains(pos.pos))

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

  private def safely[T](op: => List[T]): List[T] =
    try op catch { case ex: TypeError => Nil }

  /** Get possible completions from tree at `pos`
   *
   *  @return offset and list of symbols for possible completions
   */
  def completions(pos: SourcePosition)(implicit ctx: Context): (Int, List[Symbol]) = {
    val path = pathTo(ctx.compilationUnit.tpdTree, pos.pos)
    computeCompletions(pos, path)(contextOfPath(path))
  }

  private def computeCompletions(pos: SourcePosition, path: List[Tree])(implicit ctx: Context): (Int, List[Symbol]) = {
    val completions = Scopes.newScope.openForMutations

    /**
     * Extract basic info about completion location and the kind of symbols to include.
     *
     * @param path     The path to the position where completion happens
     * @param inImport If set, indicates that this is the completion of an import node. When
     *                 completing imports, both types and terms are always included.
     * @return The point where to insert completion, whether terms should be included in results,
     *         whether types should be included, and whether we're completing an import.
     */
    def completionInfo(path: List[Tree], inImport: Boolean): (Int, String, Boolean, Boolean, Boolean) = path match {
      case (ref: RefTree) :: _ =>
        if (ref.name == nme.ERROR)
          (ref.pos.point, "", false, false, inImport)
        else
          (ref.pos.point,
           ref.name.toString.take(pos.pos.point - ref.pos.point),
           !inImport && ref.name.isTermName, // Types and terms are always accepted in imports
           !inImport && ref.name.isTypeName,
           inImport)
      case _ =>
        (0, "", false, false, false)
    }

    val (completionPos, prefix, termOnly, typeOnly, inImport) = path match {
      case (imp: Import) :: _ =>
        imp.selectors.find(_.pos.contains(pos.pos)) match {
          case None      => (imp.expr.pos.point, "", false, false, true)
          case Some(sel) => completionInfo(sel.asInstanceOf[tpd.Tree] :: Nil, /* inImport = */ true)
        }
      case other =>
        completionInfo(other, /* inImport = */ false)
    }

    /** Include in completion sets only symbols that
     *   1. start with given name prefix, and
     *   2. do not contain '$' except in prefix where it is explicitly written by user, and
     *   3. are not a primary constructor,
     *   4. have an existing source symbol,
     *   5. are the module class in case of packages,
     *   6. are mutable accessors, to exclude setters for `var`,
     *   7. have same term/type kind as name prefix given so far
     *
     *  The reason for (2) is that we do not want to present compiler-synthesized identifiers
     *  as completion results. However, if a user explicitly writes all '$' characters in an
     *  identifier, we should complete the rest.
     *
     *  The reason for (4) is that we want to filter, for instance, non-existnet `Module`
     *  symbols that accompany class symbols. We can't simply return only the source symbols,
     *  because this would discard some synthetic symbols such as the copy method of case
     *  classes.
     */
    def include(sym: Symbol) =
      sym.name.startsWith(prefix) &&
      !sym.name.toString.drop(prefix.length).contains('$') &&
      !sym.isPrimaryConstructor &&
      sym.sourceSymbol.exists &&
      (!sym.is(Package) || !sym.moduleClass.exists) &&
      !sym.is(allOf(Mutable, Accessor)) &&
      (!termOnly || sym.isTerm) &&
      (!typeOnly || sym.isType)

    def enter(sym: Symbol) =
      if (include(sym)) completions.enter(sym)

    def add(sym: Symbol) =
      if (sym.exists && !completions.lookup(sym.name).exists) enter(sym)

    def addMember(site: Type, name: Name) =
      if (!completions.lookup(name).exists)
        for (alt <- site.member(name).alternatives) enter(alt.symbol)

    def accessibleMembers(site: Type, superAccess: Boolean = true): Seq[Symbol] = site match {
      case site: NamedType if site.symbol.is(Package) =>
        site.decls.toList.filter(include) // Don't look inside package members -- it's too expensive.
      case _ =>
        def appendMemberSyms(name: Name, buf: mutable.Buffer[SingleDenotation]): Unit =
          try buf ++= site.member(name).alternatives
          catch { case ex: TypeError => }
        site.memberDenots(takeAllFilter, appendMemberSyms).collect {
          case mbr if include(mbr.symbol) => mbr.accessibleFrom(site, superAccess).symbol
          case _ => NoSymbol
        }.filter(_.exists)
    }

    def addAccessibleMembers(site: Type, superAccess: Boolean = true): Unit =
      for (mbr <- accessibleMembers(site)) addMember(site, mbr.name)

    def getImportCompletions(ictx: Context): Unit = {
      implicit val ctx = ictx
      val imp = ctx.importInfo
      if (imp != null) {
        def addImport(name: TermName) = {
          addMember(imp.site, name)
          addMember(imp.site, name.toTypeName)
        }
        // FIXME: We need to also take renamed items into account for completions,
        // That means we have to return list of a pairs (Name, Symbol) instead of a list
        // of symbols from `completions`.!=
        for (imported <- imp.originals if !imp.excluded.contains(imported)) addImport(imported)
        if (imp.isWildcardImport)
          for (mbr <- accessibleMembers(imp.site) if !imp.excluded.contains(mbr.name.toTermName))
            addMember(imp.site, mbr.name)
      }
    }

    def getScopeCompletions(ictx: Context): Unit = {
      implicit val ctx = ictx

      if (ctx.owner.isClass) {
        addAccessibleMembers(ctx.owner.thisType)
        ctx.owner.asClass.classInfo.selfInfo match {
          case selfSym: Symbol => add(selfSym)
          case _ =>
        }
      }
      else if (ctx.scope != null) ctx.scope.foreach(add)

      getImportCompletions(ctx)

      var outer = ctx.outer
      while ((outer.owner `eq` ctx.owner) && (outer.scope `eq` ctx.scope)) {
        getImportCompletions(outer)
        outer = outer.outer
      }
      if (outer `ne` NoContext) getScopeCompletions(outer)
    }

    def implicitConversionTargets(qual: Tree)(implicit ctx: Context): Set[Type] = {
      val typer = ctx.typer
      val conversions = new typer.ImplicitSearch(defn.AnyType, qual, pos.pos).allImplicits
      val targets = conversions.map(_.widen.finalResultType)
      interactiv.println(i"implicit conversion targets considered: ${targets.toList}%, %")
      targets
    }

    def getMemberCompletions(qual: Tree): Unit = {
      addAccessibleMembers(qual.tpe)
      if (!inImport) {
        // Implicit conversions do not kick in when importing
        implicitConversionTargets(qual)(ctx.fresh.setExploreTyperState())
          .foreach(addAccessibleMembers(_))
      }
    }

    path match {
      case (sel @ Select(qual, _)) :: _ => getMemberCompletions(qual)
      case (imp @ Import(expr, _)) :: _ => getMemberCompletions(expr)
      case _  => getScopeCompletions(ctx)
    }

    val completionList = completions.toList
    interactiv.println(i"completion with pos = $pos, prefix = $prefix, termOnly = $termOnly, typeOnly = $typeOnly = $completionList%, %")
    (completionPos, completionList)
  }

  /** Possible completions of members of `prefix` which are accessible when called inside `boundary` */
  def completions(prefix: Type, boundary: Symbol)(implicit ctx: Context): List[Symbol] =
    safely {
      if (boundary != NoSymbol) {
        val boundaryCtx = ctx.withOwner(boundary)
        def exclude(sym: Symbol) = sym.isAbsent || sym.is(Synthetic) || sym.is(Artifact)
        def addMember(name: Name, buf: mutable.Buffer[SingleDenotation]): Unit =
          buf ++= prefix.member(name).altsWith(sym =>
            !exclude(sym) && sym.isAccessibleFrom(prefix)(boundaryCtx))
          prefix.memberDenots(completionsFilter, addMember).map(_.symbol).toList
      }
      else Nil
    }

  /** Filter for names that should appear when looking for completions. */
  private[this] object completionsFilter extends NameFilter {
    def apply(pre: Type, name: Name)(implicit ctx: Context): Boolean =
      !name.isConstructorName && name.toTermName.info.kind == SimpleNameKind
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

  /** Find named trees with a non-empty position whose name contains `nameSubstring` in `trees`.
   */
  def namedTrees(trees: List[SourceTree], nameSubstring: String)
   (implicit ctx: Context): List[SourceTree] = {
    val predicate: NameTree => Boolean = _.name.toString.contains(nameSubstring)
    namedTrees(trees, Include.empty, predicate)
  }

  /** Find named trees with a non-empty position satisfying `treePredicate` in `trees`.
   *
   *  @param includeReferences  If true, include references and not just definitions
   */
  def namedTrees(trees: List[SourceTree], include: Include.Set, treePredicate: NameTree => Boolean)
    (implicit ctx: Context): List[SourceTree] = safely {
    val buf = new mutable.ListBuffer[SourceTree]

    def traverser(source: SourceFile) = {
      new untpd.TreeTraverser {
        override def traverse(tree: untpd.Tree)(implicit ctx: Context) = {
          tree match {
            case imp: untpd.Import if include.isImports && tree.hasType =>
              val tree = imp.asInstanceOf[tpd.Import]
              val selections = tpd.importSelections(tree)
              traverse(imp.expr)
              selections.foreach(traverse)
            case utree: untpd.NameTree if tree.hasType =>
              val tree = utree.asInstanceOf[tpd.NameTree]
              if (tree.symbol.exists
                   && !tree.symbol.is(Synthetic)
                   && tree.pos.exists
                   && !tree.pos.isZeroExtent
                   && (include.isReferences || isDefinition(tree))
                   && treePredicate(tree))
                buf += SourceTree(tree, source)
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
      (  !tree.symbol.isPrimaryConstructor
      && (includes.isDefinitions || !Interactive.isDefinition(tree))
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
      case Some(tree) => pathTo(tree.tree, pos.pos)
      case None => Nil
    }

  def pathTo(tree: Tree, pos: Position)(implicit ctx: Context): List[Tree] =
    if (tree.pos.contains(pos))
      NavigateAST.pathTo(pos, tree, skipZeroExtent = true)
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
   * Find the definitions of the symbol at the end of `path`.
   *
   * @param path   The path to the symbol for which we want the definitions.
   * @param driver The driver responsible for `path`.
   * @return The definitions for the symbol at the end of `path`.
   */
  def findDefinitions(path: List[Tree], pos: SourcePosition, driver: InteractiveDriver)(implicit ctx: Context): List[SourceTree] = {
    enclosingSourceSymbols(path, pos).flatMap { sym =>
      val enclTree = enclosingTree(path)

      val (trees, include) =
        if (enclTree.isInstanceOf[MemberDef])
          (driver.allTreesContaining(sym.name.sourceModuleName.toString),
            Include.definitions | Include.overriding | Include.overridden)
        else sym.topLevelClass match {
          case cls: ClassSymbol =>
            val trees = Option(cls.sourceFile).flatMap(InteractiveDriver.toUriOption) match {
              case Some(uri) if driver.openedTrees.contains(uri) =>
                driver.openedTrees(uri)
              case _ => // Symbol comes from the classpath
                SourceTree.fromSymbol(cls).toList
            }
            (trees, Include.definitions | Include.overriding)
          case _ =>
            (Nil, Include.empty)
        }

      findTreesMatching(trees, include, sym)
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
        (treeSym != sym || !treeSym.is(AbstractOrTrait)) && treeSym.derivesFrom(sym)
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

}
