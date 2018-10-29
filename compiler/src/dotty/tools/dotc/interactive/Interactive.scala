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

  object Include { // should be an enum, really.
    type Set = Int
    val overridden: Int = 1 // include trees whose symbol is overridden by `sym`
    val overriding: Int = 2 // include trees whose symbol overrides `sym` (but for performance only in same source file)
    val references: Int = 4 // include references
    val definitions: Int = 8 // include definitions
    val linkedClass: Int = 16 // include `symbol.linkedClass`
    val imports: Int = 32 // include imports in the results
    val renamingImports: Int = 64 // Include renamed symbols and renaming part of imports in the results
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

    syms.map(Interactive.sourceSymbol).filter(_.exists)
  }

  /** A symbol related to `sym` that is defined in source code.
   *
   *  @see enclosingSourceSymbols
   */
  @tailrec def sourceSymbol(sym: Symbol)(implicit ctx: Context): Symbol =
    if (!sym.exists)
      sym
    else if (sym.is(ModuleVal))
      sourceSymbol(sym.moduleClass) // The module val always has a zero-extent position
    else if (sym.is(Synthetic)) {
      val linked = sym.linkedClass
      if (linked.exists && !linked.is(Synthetic))
        linked
      else
        sourceSymbol(sym.owner)
    }
    else if (sym.isPrimaryConstructor)
      sourceSymbol(sym.owner)
    else sym

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
    || sym.exists && sym == sourceSymbol(tree.symbol)
    || include != 0 && sym.name == tree.symbol.name && sym.maybeOwner != tree.symbol.maybeOwner
       && (  (include & Include.overridden) != 0 && overrides(sym, tree.symbol)
          || (include & Include.overriding) != 0 && overrides(tree.symbol, sym)
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

    val (completionPos, prefix, termOnly, typeOnly) = path match {
      case (ref: RefTree) :: _ =>
        if (ref.name == nme.ERROR)
          (ref.pos.point, "", false, false)
        else
          (ref.pos.point,
           ref.name.toString.take(pos.pos.point - ref.pos.point),
           ref.name.isTermName,
           ref.name.isTypeName)
      case _ =>
        (0, "", false, false)
    }

    /** Include in completion sets only symbols that
     *   1. start with given name prefix, and
     *   2. do not contain '$' except in prefix where it is explicitly written by user, and
     *   3. have same term/type kind as name prefix given so far
     *
     *  The reason for (2) is that we do not want to present compiler-synthesized identifiers
     *  as completion results. However, if a user explicitly writes all '$' characters in an
     *  identifier, we should complete the rest.
     */
    def include(sym: Symbol) =
      sym.name.startsWith(prefix) &&
      !sym.name.toString.drop(prefix.length).contains('$') &&
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
      implicitConversionTargets(qual)(ctx.fresh.setExploreTyperState())
        .foreach(addAccessibleMembers(_))
    }

    path match {
      case (sel @ Select(qual, _)) :: _ => getMemberCompletions(qual)
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
   (implicit ctx: Context): List[SourceNamedTree] =
    if (!sym.exists)
      Nil
    else
      namedTrees(trees, include, matchSymbol(_, sym, include))

  /** Find named trees with a non-empty position whose name contains `nameSubstring` in `trees`.
   */
  def namedTrees(trees: List[SourceTree], nameSubstring: String)
   (implicit ctx: Context): List[SourceNamedTree] = {
    val predicate: NameTree => Boolean = _.name.toString.contains(nameSubstring)
    namedTrees(trees, 0, predicate)
  }

  /** Find named trees with a non-empty position satisfying `treePredicate` in `trees`.
   *
   *  @param includeReferences  If true, include references and not just definitions
   */
  def namedTrees(trees: List[SourceTree], include: Include.Set, treePredicate: NameTree => Boolean)
    (implicit ctx: Context): List[SourceNamedTree] = safely {
    val includeReferences = (include & Include.references) != 0
    val includeImports = (include & Include.imports) != 0
    val includeRenamingImports = (include & Include.renamingImports) != 0
    val buf = new mutable.ListBuffer[SourceNamedTree]

    def traverser(source: SourceFile) = {
      new untpd.TreeTraverser {
        private def handleImport(imp: tpd.Import): Unit = {
          val imported =
            imp.selectors.flatMap {
              case id: untpd.Ident =>
                importedSymbols(imp.expr, id.name).map((_, id, None))
              case Thicket((id: untpd.Ident) :: (newName: untpd.Ident) :: Nil) =>
                val renaming = if (includeRenamingImports) Some(newName) else None
                importedSymbols(imp.expr, id.name).map((_, id, renaming))
            }
          imported match {
            case Nil =>
              traverse(imp.expr)
            case syms =>
              syms.foreach { case (sym, name, rename) =>
                val tree = tpd.Select(imp.expr, sym.name).withPos(name.pos)
                val renameTree = rename.map { r =>
                  // Get the type of the symbol that is actually selected, and construct a select
                  // node with the new name and the type of the real symbol.
                  val name = if (sym.name.isTypeName) r.name.toTypeName else r.name
                  val actual = tpd.Select(imp.expr, sym.name)
                  tpd.Select(imp.expr, name).withPos(r.pos).withType(actual.tpe)
                }
                renameTree.foreach(traverse)
                traverse(tree)
              }
          }
        }
        override def traverse(tree: untpd.Tree)(implicit ctx: Context) = {
          tree match {
            case imp: untpd.Import if includeImports =>
              handleImport(imp.asInstanceOf[tpd.Import])
            case utree: untpd.NameTree if tree.hasType =>
              val tree = utree.asInstanceOf[tpd.NameTree]
              if (tree.symbol.exists
                   && !tree.symbol.is(Synthetic)
                   && tree.pos.exists
                   && !tree.pos.isZeroExtent
                   && (includeReferences || isDefinition(tree))
                   && treePredicate(tree))
                buf += SourceNamedTree(tree, source)
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
   * @param trees    The trees to inspect.
   * @param includes Whether to include references, definitions, etc.
   * @param symbol   The symbol for which we want to find references.
   */
  def findTreesMatching(trees: List[SourceTree],
                        includes: Include.Set,
                        symbol: Symbol)(implicit ctx: Context): List[SourceNamedTree] = {
    val linkedSym = symbol.linkedClass
    val includeDeclaration = (includes & Include.definitions) != 0
    val includeLinkedClass = (includes & Include.linkedClass) != 0
    val includeRenamingImports = (includes & Include.renamingImports) != 0
    val predicate: NameTree => Boolean = tree =>
      (  !tree.symbol.isPrimaryConstructor
      && (includeDeclaration || !Interactive.isDefinition(tree))
      && (includeRenamingImports || !isRenamed(tree))
      && (  Interactive.matchSymbol(tree, symbol, includes)
         || (  includeDeclaration
            && includeLinkedClass
            && linkedSym.exists
            && Interactive.matchSymbol(tree, linkedSym, includes)
            )
         )
      )
    namedTrees(trees, includes, predicate)
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
  def findDefinitions(path: List[Tree], pos: SourcePosition, driver: InteractiveDriver)(implicit ctx: Context): List[SourceNamedTree] = {
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
            (Nil, 0)
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
   * All the symbols that are imported by import statement `imp`, if it matches
   * the predicate `selectorPredicate`.
   *
   * @param imp The import statement to analyze
   * @param selectorPredicate A test to find the selector to use.
   * @return The symbols imported.
   */
   private def importedSymbols(imp: tpd.Import,
                       selectorPredicate: untpd.Tree => Boolean = util.common.alwaysTrue)
                      (implicit ctx: Context): List[Symbol] = {
     val symbols = imp.selectors.find(selectorPredicate) match {
       case Some(id: untpd.Ident) =>
         importedSymbols(imp.expr, id.name)
       case Some(Thicket((id: untpd.Ident) :: (_: untpd.Ident) :: Nil)) =>
         importedSymbols(imp.expr, id.name)
       case _ =>
         Nil
     }

     symbols.map(sourceSymbol).filter(_.exists).distinct
   }

   /**
    * The symbols that are imported with `expr.name`
    *
    * @param expr The base of the import statement
    * @param name The name that is being imported.
    * @return All the symbols that would be imported with `expr.name`.
    */
   private def importedSymbols(expr: tpd.Tree, name: Name)(implicit ctx: Context): List[Symbol] = {
     def lookup(name: Name): Symbol = expr.tpe.member(name).symbol
       List(lookup(name.toTermName),
            lookup(name.toTypeName),
            lookup(name.moduleClassName),
            lookup(name.sourceModuleName))
   }

  /**
   * Is this tree using a renaming introduced by an import statement?
   *
   * @param tree The tree to inspect
   * @return True, if this tree's name is different than its symbol's name, indicating that
   *         it uses a renaming introduced by an import statement.
   */
  def isRenamed(tree: NameTree)(implicit ctx: Context): Boolean = {
    val symbol = tree.symbol
    symbol.exists && !sameName(tree.name, symbol.name)
  }

  /** Are the two names the same? */
  def sameName(n0: Name, n1: Name): Boolean = {
    n0.stripModuleClassSuffix.toString == n1.stripModuleClassSuffix.toString
  }

  /**
   * Is this tree immediately enclosing an import that renames a symbol to `toName`?
   *
   * @param toName        The target name to check
   * @param tree          The tree to check
   * @return True if this tree immediately encloses an import that renames a symbol to `toName`,
   *         false otherwise.
   */
  def immediatelyEnclosesRenaming(toName: Name, inTree: Tree)(implicit ctx: Context): Boolean = {
    def isImportRenaming(tree: Tree): Boolean = {
      tree match {
        case Import(_, selectors) =>
          selectors.exists {
            case Thicket(_ :: Ident(rename) :: Nil) =>
              rename.stripModuleClassSuffix.toString == toName.stripModuleClassSuffix.toString
            case _ =>
              false
          }
            case _ =>
              false
      }
    }

    inTree match {
      case PackageDef(_, stats) =>
        stats.exists(isImportRenaming)
      case template: Template =>
        template.body.exists(isImportRenaming)
      case Block(stats, _) =>
        stats.exists(isImportRenaming)
      case _ =>
        false
    }
  }

  /**
   * In `enclosing`, find all the references to any of `syms` that have been renamed to `toName`.
   *
   * If `enclosing` is empty, it means the renaming import was top-level and the whole source file
   * should be considered. Otherwise, we can restrict the search to this tree because renaming
   * imports are local.
   *
   * @param toName    The name that is set by the renaming.
   * @param enclosing The tree that encloses the renaming import, if it exists.
   * @param syms      The symbols to which we want to find renamed references.
   * @param allTrees  All the trees in this source file, in case we can't find `enclosing`.
   * @param source    The sourcefile that where to look for references.
   * @return All the references to the symbol under the cursor that are using `toName`.
   */
  def findTreesMatchingRenaming(toName: Name,
                                enclosing: Option[Tree],
                                syms: List[Symbol],
                                allTrees: List[Tree],
                                source: SourceFile
                               )(implicit ctx: Context): List[SourceNamedTree] = {

    /**
     * Remove the blocks that immediately enclose a renaming to `toName` in `inTree`.
     *
     * @param toName The target name of renamings.
     * @param inTree The tree in which to remove the blocks that have such a renaming.
     * @return A tree that has no children containing a renaming to `toName`.
     */
    def removeBlockWithRenaming(toName: Name, inTree: Tree): Tree = {
      new TreeMap {
        override def transform(tree: Tree)(implicit ctx: Context): Tree = tree match {
          case pkg: PackageDef if immediatelyEnclosesRenaming(toName, pkg) =>
            EmptyTree
          case template: Template if immediatelyEnclosesRenaming(toName, template) =>
            cpy.Template(template)(constr = DefDef(template.constr.symbol.asTerm), self = template.self, body = Nil)
          case block @ Block(stats, expr) if immediatelyEnclosesRenaming(toName, block) =>
            EmptyTree
          case other =>
            super.transform(other)
        }
      }.transform(inTree)
    }

    val trees = {
      val trees = enclosing match {
        case Some(pkg: PackageDef) =>
          pkg.stats
        case Some(template: Template) =>
          template.body
        case Some(block: Block) =>
          block.expr :: block.stats
        case _ =>
          // No enclosing tree; we'll search in the whole file.
          allTrees
      }

      // These trees may contain a new renaming of the same symbol to the same name, so we may
      // have to cut some branches
      val trimmedTrees = trees.map(removeBlockWithRenaming(toName, _))

      // Some of these trees may not be `NameTrees`. Those that are not are wrapped in a
      // synthetic val def, so that everything can go inside `SourceNamedTree`s.
      trimmedTrees.map {
        case tree: NameTree =>
          SourceNamedTree(tree, source)
        case tree =>
          val valDef = tpd.SyntheticValDef(NameKinds.UniqueName.fresh(), tree)
          SourceNamedTree(valDef, source)
      }
    }

    val includes =
      Include.references | Include.imports | Include.renamingImports

    syms.flatMap { sym =>
      Interactive.namedTrees(trees,
        includes,
        tree =>
          Interactive.sameName(tree.name, toName) &&
          (Interactive.matchSymbol(tree, sym, includes) || Interactive.matchSymbol(tree, sym.linkedClass, includes)))
    }
  }

}
