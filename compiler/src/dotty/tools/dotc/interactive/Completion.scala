package dotty.tools.dotc.interactive

import java.nio.charset.Charset

import dotty.tools.dotc.ast.Trees._
import dotty.tools.dotc.ast.untpd
import dotty.tools.dotc.config.Printers.interactiv
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.CheckRealizable
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Denotations.SingleDenotation
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Names.{Name, TermName}
import dotty.tools.dotc.core.NameKinds.SimpleNameKind
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.Symbols.{NoSymbol, Symbol, TermSymbol, defn, newSymbol}
import dotty.tools.dotc.core.Scopes
import dotty.tools.dotc.core.StdNames.{nme, tpnme}
import dotty.tools.dotc.core.SymDenotations.SymDenotation
import dotty.tools.dotc.core.TypeComparer
import dotty.tools.dotc.core.TypeError
import dotty.tools.dotc.core.Types.{ExprType, MethodOrPoly, NameFilter, NamedType, NoType, PolyType, TermRef, Type}
import dotty.tools.dotc.printing.Texts._
import dotty.tools.dotc.util.{NameTransformer, NoSourcePosition, SourcePosition}

import scala.collection.mutable

/**
 * One of the results of a completion query.
 *
 * @param label         The label of this completion result, or the text that this completion result
 *                      should insert in the scope where the completion request happened.
 * @param description   The description of this completion result: the fully qualified name for
 *                      types, or the type for terms.
 * @param symbols       The symbols that are matched by this completion result.
 */
case class Completion(label: String, description: String, symbols: List[Symbol])

object Completion {

  import dotty.tools.dotc.ast.tpd._

  /** Get possible completions from tree at `pos`
   *
   *  @return offset and list of symbols for possible completions
   */
  def completions(pos: SourcePosition)(using Context): (Int, List[Completion]) = {
    val path = Interactive.pathTo(ctx.compilationUnit.tpdTree, pos.span)
    computeCompletions(pos, path)(using Interactive.contextOfPath(path))
  }

  /**
   * Inspect `path` to determine what kinds of symbols should be considered.
   *
   * If the path starts with:
   *  - a `RefTree`, then accept symbols of the same kind as its name;
   *  - a renaming import, and the cursor is on the renamee, accept both terms and types;
   *  - an import, accept both terms and types;
   *
   * Otherwise, provide no completion suggestion.
   */
  private def completionMode(path: List[Tree], pos: SourcePosition): Mode =
    path match {
      case (ref: RefTree) :: _ =>
        if (ref.name.isTermName) Mode.Term
        else if (ref.name.isTypeName) Mode.Type
        else Mode.None

      case (sel: untpd.ImportSelector) :: _ =>
        if sel.imported.span.contains(pos.span) then Mode.Import
        else Mode.None // Can't help completing the renaming

      case Import(_, _) :: _ =>
        Mode.Import

      case _ =>
        Mode.None
    }

  /**
   * Inspect `path` to determine the completion prefix. Only symbols whose name start with the
   * returned prefix should be considered.
   */
  private def completionPrefix(path: List[untpd.Tree], pos: SourcePosition): String =
    path match {
      case (sel: untpd.ImportSelector) :: _ =>
        completionPrefix(sel.imported :: Nil, pos)

      case Import(expr, selectors) :: _ =>
        selectors.find(_.span.contains(pos.span)).map { selector =>
          completionPrefix(selector :: Nil, pos)
        }.getOrElse("")

      case (ref: untpd.RefTree) :: _ =>
        if (ref.name == nme.ERROR) ""
        else ref.name.toString.take(pos.span.point - ref.span.point)

      case _ =>
        ""
    }

  /** Inspect `path` to determine the offset where the completion result should be inserted. */
  private def completionOffset(path: List[Tree]): Int =
    path match {
      case (ref: RefTree) :: _ => ref.span.point
      case _ => 0
    }

  /** Create a new `CompletionBuffer` for completing at `pos`. */
  private def completionBuffer(path: List[Tree], pos: SourcePosition): CompletionBuffer = {
    val mode = completionMode(path, pos)
    val prefix = completionPrefix(path, pos)
    new CompletionBuffer(mode, prefix, pos)
  }

  private def computeCompletions(pos: SourcePosition, path: List[Tree])(using Context): (Int, List[Completion]) = {

    val offset = completionOffset(path)
    val buffer = completionBuffer(path, pos)

    val scope = path match {
        case Select(qual, _) :: _                              => buffer.selectionCompletions(path, qual)
        case Import(expr, _) :: _                              => buffer.directMemberCompletions(expr)
        case (_: untpd.ImportSelector) :: Import(expr, _) :: _ => buffer.directMemberCompletions(expr)
        case _                                                 => buffer.scopeCompletions
      }

    val completionList = scope.getCompletions

    interactiv.println(i"""completion with pos     = $pos,
                          |                prefix  = ${buffer.prefix},
                          |                term    = ${buffer.mode.is(Mode.Term)},
                          |                type    = ${buffer.mode.is(Mode.Type)}
                          |                results = $completionList%, %""")
    (offset, completionList)
  }

  /**
   * A description for completion result that represents `symbols`.
   *
   * If `denots` contains a single denotation, show its full name in case it's a type, or its type if
   * it's a term.
   *
   * When there are multiple denotations, show their kinds.
   */
  private def description(denots: List[SingleDenotation])(using Context): String =
    denots match {
      case denot :: Nil =>
        if (denot.isType) denot.symbol.showFullName
        else denot.info.widenTermRefExpr.show

      case denot :: _ =>
        denots.map(den => ctx.printer.kindString(den.symbol)).distinct.mkString("", " and ", s" ${denot.symbol.name.stripModuleClassSuffix.show}")

      case Nil =>
        ""
    }

  private class CompletionBuffer(val mode: Mode, val prefix: String, pos: SourcePosition) {
    /** Completions for terms and types that are currently in scope:
     *  the members of the current class and the symbols that have been imported, recursively adding completions from outer scopes
     */
    def scopeCompletions(using Context): CompletionScope = {
      extension[A, K](elems: Seq[A])
        def orderedGroupBy(f: A => K): Seq[(K, Seq[A])] =
          val keys = elems.map(f).distinct
          val grouped = elems.groupBy(f)
          keys.map(key => key -> grouped(key))

      val grouped = ctx.outersIterator.toList.reverse.orderedGroupBy(_.owner).filter(_._1.exists)
      val imported = grouped.map { (owner, contexts) =>
        contexts.collect { case context if context.isImportContext =>
          importedCompletions(using context)
        }.foldLeft(CompletionScope.empty)(_.mergeDiscardingAmbiguities(_))
      }
      val members = grouped.map { (owner, _) =>
        if owner.isClass then
          CompletionScope.from(accessibleMembers(owner.thisType))
        else CompletionScope.empty
      }

      (imported ++ members).foldLeft(CompletionScope.empty)(_.mergeShadowedBy(_))
    }

    def selectionCompletions(path: List[Tree], qual: Tree)(using Context): CompletionScope =
      implicitConversionMemberCompletions(qual)
        .mergeShadowedBy(extensionCompletions(path, qual))
        .mergeShadowedBy(directMemberCompletions(qual))

    /** Completions for members of `qual`'s type.
     *  These include inherited definitions but not members added by extensions or implicit conversions
     */
    def directMemberCompletions(qual: Tree)(using Context): CompletionScope =
      if qual.tpe.widenDealias.isExactlyNothing then
        CompletionScope.empty
      else
        CompletionScope.from(accessibleMembers(qual.tpe))

    /** Completions introduced by imports directly in this context.
     *  Completions from outer contexts are not included.
     */
    private def importedCompletions(using Context): CompletionScope = {
      val imp = ctx.importInfo
      if imp == null then
        CompletionScope.empty
      else {
        def fromImport(name: TermName, nameInScope: TermName) =
          val terms = imp.site.member(name).alternatives.map(denot => nameInScope -> denot)
          val types = imp.site.member(name.toTypeName).alternatives.map(denot => nameInScope.toTypeName -> denot)
          CompletionScope.fromNamed(terms ++ types)

        val givenImports = CompletionScope.fromNamed(imp.importedImplicits.map(x => (x.implicitName, x.underlyingRef.denot.asSingleDenotation)))

        val wildcardMembers =
          if imp.selectors.exists(_.imported.name == nme.WILDCARD) then
            val denots = accessibleMembers(imp.site)
              .filter(mbr => !mbr.symbol.is(Given) && !imp.excluded.contains(mbr.name.toTermName))
            CompletionScope.from(denots)
          else
            CompletionScope.empty
        val explicitMembers =
          imp.reverseMapping.toList.collect {
            case (nameInScope, original) if original != nameInScope || !imp.excluded.contains(original) =>
              fromImport(original, nameInScope)
          }.foldLeft(CompletionScope.empty)(_.mergeDiscardingAmbiguities(_))

        givenImports
          .mergeShadowedBy(wildcardMembers)
          .mergeShadowedBy(explicitMembers)
      }
    }

    private def implicitConversionMemberCompletions(qual: Tree)(using Context): CompletionScope =
      if qual.tpe.widenDealias.isExactlyNothing || qual.tpe.isNullType then
        CompletionScope.empty
      else
        val membersFromConversion =
          implicitConversionTargets(qual)(using ctx.fresh.setExploreTyperState()).flatMap(accessibleMembers)
        CompletionScope.from(membersFromConversion.toSeq)

    private def extensionCompletions(path: List[Tree], qual: Tree)(using Context): CompletionScope =
      def asDefLikeType(tpe: Type): Type = tpe match
        case _: MethodOrPoly => tpe
        case _ => ExprType(tpe)

      def tryApplyingReceiver(termRef: TermRef): Option[SingleDenotation] =
        ctx.typer.tryApplyingReceiver(termRef, qual)
          .map { tree =>
            val tpe = asDefLikeType(tree.tpe.dealias)
            termRef.denot.asSingleDenotation.mapInfo(_ => tpe)
          }

      val matchingNamePrefix = completionPrefix(path, pos)

      def extractMemberExtensionMethods(types: Seq[Type]): Seq[(TermRef, TermName)] =
        object DenotWithMatchingName:
          def unapply(denot: SingleDenotation): Option[(SingleDenotation, TermName)] =
            denot.name match
              case name: TermName if name.startsWith(matchingNamePrefix) => Some((denot, name))
              case _ => None

        types.flatMap{ tpe =>
          tpe.membersBasedOnFlags(required = ExtensionMethod, excluded = EmptyFlags)
            .collect { case DenotWithMatchingName(denot, name) => TermRef(tpe, denot.symbol) -> name }
        }

      // There are four possible ways for an extension method to be applicable

      // 1. The extension method is visible under a simple name, by being defined or inherited or imported in a scope enclosing the reference.
      val extMethodsInScope = scopeCompletions.nameToDenots.toList.flatMap {
        case (name, denots) => denots.collect { case d: SymDenotation => (d.termRef, name.asTermName) }
      }

      // 2. The extension method is a member of some given instance that is visible at the point of the reference.
      val givensInScope = ctx.implicits.eligible(defn.AnyType).map(_.implicitRef.underlyingRef)
      val extMethodsFromGivensInScope = extractMemberExtensionMethods(givensInScope)

      // 3. The reference is of the form r.m and the extension method is defined in the implicit scope of the type of r.
      val implicitScopeCompanions = ctx.run.implicitScope(qual.tpe).companionRefs.showAsList
      val extMethodsFromImplicitScope = extractMemberExtensionMethods(implicitScopeCompanions)

      // 4. The reference is of the form r.m and the extension method is defined in some given instance in the implicit scope of the type of r.
      val givensInImplicitScope = implicitScopeCompanions.flatMap(_.membersBasedOnFlags(required = Given, excluded = EmptyFlags)).map(_.info)
      val extMethodsFromGivensInImplicitScope = extractMemberExtensionMethods(givensInImplicitScope)

      val availableExtMethods = extMethodsFromGivensInImplicitScope ++ extMethodsFromImplicitScope ++ extMethodsFromGivensInScope ++ extMethodsInScope
      val extMethodsWithAppliedReceiver = availableExtMethods.flatMap {
        case (termRef, termName) =>
          if termRef.symbol.is(ExtensionMethod) && !qual.tpe.isBottomType then
            val applied = tryApplyingReceiver(termRef)
            applied.map{ denot =>
              val sym = denot.symbol.asTerm.copy(name = termName)
              denot.derivedSingleDenotation(sym, denot.info)
            }
          else None
      }
      CompletionScope.from(extMethodsWithAppliedReceiver)

    /** Include in completion sets only symbols that
     *   1. start with given name prefix, and
     *   2. is not absent (info is not NoType)
     *   3. are not a primary constructor,
     *   4. have an existing source symbol,
     *   5. are the module class in case of packages,
     *   6. are mutable accessors, to exclude setters for `var`,
     *   7. symbol is not a package object
     *   8. symbol is not an artifact of the compiler
     *   9. have same term/type kind as name prefix given so far
     */
    private def include(sym: Symbol, nameInScope: Name)(using Context): Boolean =
      nameInScope.startsWith(prefix) &&
      sym.exists &&
      completionsFilter(NoType, nameInScope) &&
      !sym.isAbsent() &&
      !sym.isPrimaryConstructor &&
      sym.sourceSymbol.exists &&
      (!sym.is(Package) || sym.is(ModuleClass)) &&
      !sym.isAllOf(Mutable | Accessor) &&
      !sym.isPackageObject &&
      !sym.is(Artifact) &&
      (
           (mode.is(Mode.Term) && sym.isTerm)
        || (mode.is(Mode.Type) && (sym.isType || sym.isStableMember))
      )

    /** @param site The type to inspect.
     *  @return The members of `site` that are accessible and pass the include filter.
     */
    private def accessibleMembers(site: Type)(using Context): Seq[SingleDenotation] = site match {
      case site: NamedType if site.symbol.is(Package) =>
        extension (tpe: Type)
          def accessibleMembers = tpe.allMembers.toList.filter(denot => denot.symbol.isAccessibleFrom(site, superAccess = false))

        val packageDecls = site.accessibleMembers
        val packageObjectsDecls = packageDecls.filter(_.symbol.isPackageObject).flatMap(_.symbol.thisType.accessibleMembers)

        packageDecls ++ packageObjectsDecls
      case _ =>
        def appendMemberSyms(name: Name, buf: mutable.Buffer[SingleDenotation]): Unit =
          try buf ++= site.member(name).alternatives
          catch { case ex: TypeError => }
        site.memberDenots(completionsFilter, appendMemberSyms).collect {
          case mbr if include(mbr.symbol, mbr.symbol.name)
                      && mbr.symbol.isAccessibleFrom(site, superAccess = true) => mbr
        }
    }

    /**
     * Given `qual` of type T, finds all the types S such that there exists an implicit conversion
     * from T to S.
     *
     * @param qual The argument to which the implicit conversion should be applied.
     * @return The set of types that `qual` can be converted to.
     */
    private def implicitConversionTargets(qual: Tree)(using Context): Set[Type] = {
      val typer = ctx.typer
      val conversions = new typer.ImplicitSearch(defn.AnyType, qual, pos.span).allImplicits
      val targets = conversions.map(_.widen.finalResultType)
      interactiv.println(i"implicit conversion targets considered: ${targets.toList}%, %")
      targets
    }

    /** Filter for names that should appear when looking for completions. */
    private object completionsFilter extends NameFilter {
      def apply(pre: Type, name: Name)(using Context): Boolean =
        !name.isConstructorName && name.toTermName.info.kind == SimpleNameKind
      def isStable = true
    }

    extension (scope: CompletionScope)
      private def withDenots(denotations: Seq[SingleDenotation], name: Name)(using Context): CompletionScope = {
        val denots = denotations.filter(denot => include(denot.symbol, name))
        val shortName = name.stripModuleClassSuffix

        if denots.nonEmpty then
          CompletionScope(scope.nameToDenots + (shortName -> denots.toList))
        else
          scope
      }

    extension (scope: CompletionScope.type)
      private def from(denots: Seq[SingleDenotation])(using Context): CompletionScope = {
        val mappings = denots.filter(den => include(den.symbol, den.name)).toList.groupBy(_.name).map( (name, denots) => name.stripModuleClassSuffix -> denots)
        CompletionScope(mappings)
      }

      private def fromNamed(namedDenots: Seq[(Name, SingleDenotation)])(using Context): CompletionScope = {
        val mappings = namedDenots.filter((name, den) => include(den.symbol, name)).toList.groupBy(_._1).map( (name, namedDens) => name.stripModuleClassSuffix -> namedDens.map(_._2))
        CompletionScope(mappings)
      }
  }

  /**
   * The completion mode: defines what kinds of symbols should be included in the completion
   * results.
   */
  private class Mode(val bits: Int) extends AnyVal {
    def is(other: Mode): Boolean = (bits & other.bits) == other.bits
    def |(other: Mode): Mode = new Mode(bits | other.bits)
  }
  private object Mode {
    /** No symbol should be included */
    val None: Mode = new Mode(0)

    /** Term symbols are allowed */
    val Term: Mode = new Mode(1)

    /** Type and stable term symbols are allowed */
    val Type: Mode = new Mode(2)

    /** Both term and type symbols are allowed */
    val Import: Mode = new Mode(4) | Term | Type
  }

  private object CompletionScope  {
    val empty = CompletionScope()
  }

  private case class CompletionScope(nameToDenots: Map[Name, List[SingleDenotation]] = Map.empty) {
    def mergeShadowedBy(that: CompletionScope) = CompletionScope(this.nameToDenots ++ that.nameToDenots)

    def mergeDiscardingAmbiguities(that: CompletionScope) =
      val mappings = (this.nameToDenots.toList ++ that.nameToDenots.toList)
        .groupBy(_._1)
        .collect {
          case (name, (_, denots) :: Nil)  => name -> denots
        }
      CompletionScope(mappings)

    /**
     * Return the list of symbols that should be included in completion results.
     *
     * If several symbols share the same name, the type symbols appear before term symbols inside
     * the same `Completion`.
     */
    def getCompletions(using Context): List[Completion] = {
      nameToDenots.toList.groupBy(_._1.toTermName.show).map { (name, namedDenots) =>
        val typesFirst = namedDenots.flatMap(_._2).sortWith((s1, s2) => s1.isType && !s2.isType)
        val desc = description(typesFirst)
        Completion(name, desc, typesFirst.map(_.symbol))
      }.toList
    }
  }
}

