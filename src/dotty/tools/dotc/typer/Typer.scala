package dotty.tools
package dotc
package typer

import core._
import ast._
import Trees._
import Constants._
import StdNames._
import Scopes._
import Denotations._
import ProtoTypes._
import Contexts._
import Symbols._
import Types._
import SymDenotations._
import Annotations._
import Names._
import NameOps._
import Flags._
import Decorators._
import ErrorReporting._
import Checking._
import EtaExpansion.etaExpand
import dotty.tools.dotc.transform.Erasure.Boxing
import util.Positions._
import util.common._
import util.SourcePosition
import collection.mutable
import annotation.tailrec
import Implicits._
import util.Stats.{track, record}
import config.Printers._
import language.implicitConversions

trait TyperContextOps { ctx: Context => }

object Typer {

  /** The precedence of bindings which determines which of several bindings will be
   *  accessed by an Ident.
   */
  object BindingPrec {
    val definition = 4
    val namedImport = 3
    val wildImport = 2
    val packageClause = 1
    val nothingBound = 0
    def isImportPrec(prec: Int) = prec == namedImport || prec == wildImport
  }

  /** Assert tree has a position, unless it is empty or a typed splice */
  def assertPositioned(tree: untpd.Tree)(implicit ctx: Context) =
    if (!tree.isEmpty && !tree.isInstanceOf[untpd.TypedSplice] && ctx.typerState.isGlobalCommittable)
      assert(tree.pos.exists, s"position not set for $tree # ${tree.uniqueId}")
}

class Typer extends Namer with TypeAssigner with Applications with Implicits with Inferencing with Checking {

  import Typer._
  import tpd.{cpy => _, _}
  import untpd.cpy

  /** A temporary data item valid for a single typed ident:
   *  The set of all root import symbols that have been
   *  encountered as a qualifier of an import so far.
   *  Note: It would be more proper to move importedFromRoot into typedIdent.
   *  We should check that this has no performance degradation, however.
   */
  private var importedFromRoot: Set[Symbol] = Set()

  /** Attribute an identifier consisting of a simple name or wildcard
   *
   *  @param tree      The tree representing the identifier.
   *  Transformations: (1) Prefix class members with this.
   *                   (2) Change imported symbols to selections.
   *                   (3) Change pattern Idents id (but not wildcards) to id @ _
   */
  def typedIdent(tree: untpd.Ident, pt: Type)(implicit ctx: Context): Tree = track("typedIdent") {
    val refctx = ctx
    val name = tree.name

    /** Method is necessary because error messages need to bind to
     *  to typedIdent's context which is lost in nested calls to findRef
     */
    def error(msg: => String, pos: Position) = ctx.error(msg, pos)

    /** Is this import a root import that has been shadowed by an explicit
     *  import in the same program?
     */
    def isDisabled(imp: ImportInfo, site: Type): Boolean = {
      if (imp.isRootImport && (importedFromRoot contains site.termSymbol)) return true
      if (imp.hiddenRoot.exists) importedFromRoot += imp.hiddenRoot
      false
    }

    /** Does this identifier appear as a constructor of a pattern? */
    def isPatternConstr =
      if (ctx.mode.isExpr && (ctx.outer.mode is Mode.Pattern))
        ctx.outer.tree match {
          case Apply(`tree`, _) => true
          case _ => false
        }
      else false

    /** A symbol qualifies if it really exists. In addition,
     *  if we are in a constructor of a pattern, we ignore all definitions
     *  which are methods and not accessors (note: if we don't do that
     *  case x :: xs in class List would return the :: method).
     */
    def qualifies(denot: Denotation): Boolean =
      reallyExists(denot) && !(
         pt.isInstanceOf[UnapplySelectionProto] &&
         (denot.symbol is (Method, butNot = Accessor)))

    /** Find the denotation of enclosing `name` in given context `ctx`.
     *  @param previous    A denotation that was found in a more deeply nested scope,
     *                     or else `NoDenotation` if nothing was found yet.
     *  @param prevPrec    The binding precedence of the previous denotation,
     *                     or else `nothingBound` if nothing was found yet.
     *  @param prevCtx     The context of the previous denotation,
     *                     or else `NoContext` if nothing was found yet.
     */
    def findRef(previous: Type, prevPrec: Int, prevCtx: Context)(implicit ctx: Context): Type = {
      import BindingPrec._

      /** A string which explains how something was bound; Depending on `prec` this is either
       *      imported by <tree>
       *  or  defined in <symbol>
       */
      def bindingString(prec: Int, whereFound: Context, qualifier: String = "") =
        if (prec == wildImport || prec == namedImport) d"imported$qualifier by ${whereFound.importInfo}"
        else d"defined$qualifier in ${whereFound.owner}"

      /** Check that any previously found result from an inner context
       *  does properly shadow the new one from an outer context.
       */
      def checkNewOrShadowed(found: Type, newPrec: Int): Type =
        if (!previous.exists || ctx.typeComparer.isSameRef(previous, found)) found
        else if ((prevCtx.scope eq ctx.scope) &&
                 (newPrec == definition ||
                  newPrec == namedImport && prevPrec == wildImport)) {
          // special cases: definitions beat imports, and named imports beat
          // wildcard imports, provided both are in contexts with same scope
          found
        }
        else {
          if (!previous.isError && !found.isError) {
            error(
              d"""reference to $name is ambiguous;
                 |it is both ${bindingString(newPrec, ctx, "")}
                 |and ${bindingString(prevPrec, prevCtx, " subsequently")}""".stripMargin,
              tree.pos)
          }
          previous
        }

      /** The type representing a named import with enclosing name when imported
       *  from given `site` and `selectors`.
       */
      def namedImportRef(site: Type, selectors: List[untpd.Tree]): Type = {
        def checkUnambiguous(found: Type) = {
          val other = namedImportRef(site, selectors.tail)
          if (other.exists && found.exists && (found != other))
            error(d"reference to $name is ambiguous; it is imported twice in ${ctx.tree}",
                  tree.pos)
          found
        }
        val Name = name.toTermName
        selectors match {
          case Pair(Ident(from), Ident(Name)) :: rest =>
            val selName = if (name.isTypeName) from.toTypeName else from
            // Pass refctx so that any errors are reported in the context of the
            // reference instead of the context of the import.
            checkUnambiguous(selectionType(site, selName, tree.pos)(refctx))
          case Ident(Name) :: rest =>
            checkUnambiguous(selectionType(site, name, tree.pos)(refctx))
          case _ :: rest =>
            namedImportRef(site, rest)
          case nil =>
            NoType
        }
      }

      /** The type representing a wildcard import with enclosing name when imported
       *  from given import info
       */
      def wildImportRef(imp: ImportInfo): Type = {
        if (imp.isWildcardImport) {
          val pre = imp.site
          if (!isDisabled(imp, pre) && !(imp.excluded contains name.toTermName)) {
            val denot = pre.member(name).accessibleFrom(pre)(refctx)
            if (reallyExists(denot)) return pre.select(name, denot)
          }
        }
        NoType
      }

      /** Is (some alternative of) the given predenotation `denot`
       *  defined in current compilation unit?
       */
      def isDefinedInCurrentUnit(denot: Denotation): Boolean = denot match {
        case MultiDenotation(d1, d2) => isDefinedInCurrentUnit(d1) || isDefinedInCurrentUnit(d2)
        case denot: SingleDenotation => denot.symbol.sourceFile == ctx.source
      }

      /** Is `denot` the denotation of a self symbol? */
      def isSelfDenot(denot: Denotation) = denot match {
        case denot: SymDenotation => denot is SelfName
        case _ => false
      }

      // begin findRef
      if (ctx.scope == null) previous
      else {
        val outer = ctx.outer
        if ((ctx.scope ne outer.scope) || (ctx.owner ne outer.owner)) {
          val defDenot = ctx.denotNamed(name)
          if (qualifies(defDenot)) {
            val curOwner = ctx.owner
            val found =
              if (isSelfDenot(defDenot)) curOwner.enclosingClass.thisType
              else curOwner.thisType.select(name, defDenot)
            if (!(curOwner is Package) || (defDenot.symbol is Package) || isDefinedInCurrentUnit(defDenot))
              return checkNewOrShadowed(found, definition) // no need to go further out, we found highest prec entry
            else if (prevPrec < packageClause)
              return findRef(found, packageClause, ctx)(outer)
          }
        }
        val curImport = ctx.importInfo
        if (curImport != null && curImport.isRootImport && previous.exists) return previous
        // would import of kind `prec` be not shadowed by a nested higher-precedence definition?
        def isPossibleImport(prec: Int) =
          prevPrec < prec || prevPrec == prec && (prevCtx.scope eq ctx.scope)
        if (isPossibleImport(namedImport) && (curImport ne outer.importInfo) && !curImport.sym.isCompleting) {
          val namedImp = namedImportRef(curImport.site, curImport.selectors)
          if (namedImp.exists)
            return findRef(checkNewOrShadowed(namedImp, namedImport), namedImport, ctx)(outer)
          if (isPossibleImport(wildImport)) {
            val wildImp = wildImportRef(curImport)
            if (wildImp.exists)
              return findRef(checkNewOrShadowed(wildImp, wildImport), wildImport, ctx)(outer)
          }
        }
        findRef(previous, prevPrec, prevCtx)(outer)
      }
    }

    // begin typedIdent
    def kind = if (name.isTermName) "" else "type "
    typr.println(s"typed ident $kind$name in ${ctx.owner}")
    if (ctx.mode is Mode.Pattern) {
      if (name == nme.WILDCARD)
        return tree.withType(pt)
      if (isVarPattern(tree))
        return typed(untpd.Bind(name, untpd.Ident(nme.WILDCARD)).withPos(tree.pos), pt)
    }

    val saved = importedFromRoot
    importedFromRoot = Set.empty

    val rawType =
      try findRef(NoType, BindingPrec.nothingBound, NoContext)
      finally importedFromRoot = saved

    val ownType =
      if (rawType.exists)
        ensureAccessible(rawType, superAccess = false, tree.pos)
      else {
        error(d"not found: $kind$name", tree.pos)
        ErrorType
      }

    val tree1 = ownType match {
      case ownType: NamedType if !prefixIsElidable(ownType) =>
        ref(ownType).withPos(tree.pos)
      case _ =>
        tree.withType(ownType)
    }

    checkValue(tree1, pt)
  }

  def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = track("typedSelect") {
    def asSelect(implicit ctx: Context): Tree = {
      val qual1 = typedExpr(tree.qualifier, selectionProto(tree.name, pt, this))
      if (tree.name.isTypeName) checkStable(qual1.tpe, qual1.pos)
      checkValue(assignType(cpy.Select(tree)(qual1, tree.name), qual1), pt)
    }

    def asJavaSelectFromTypeTree(implicit ctx: Context): Tree = {
      // Translate names in Select/Ident nodes to type names.
      def convertToTypeName(tree: untpd.Tree): Option[untpd.Tree] = tree match {
        case Select(qual, name) => Some(untpd.Select(qual, name.toTypeName))
        case Ident(name)        => Some(untpd.Ident(name.toTypeName))
        case _                  => None
      }

      // Try to convert Select(qual, name) to a SelectFromTypeTree.
      def convertToSelectFromType(qual: untpd.Tree, origName: Name): Option[untpd.SelectFromTypeTree] =
        convertToTypeName(qual) match {
          case Some(qual1)  => Some(untpd.SelectFromTypeTree(qual1 withPos qual.pos, origName.toTypeName))
          case _            => None
        }

      convertToSelectFromType(tree.qualifier, tree.name) match {
        case Some(sftt) => typedSelectFromTypeTree(sftt, pt)
        case _ => ctx.error(d"Could not convert $tree to a SelectFromTypeTree"); EmptyTree
      }
    }

    if(ctx.compilationUnit.isJava && tree.name.isTypeName) {
      // SI-3120 Java uses the same syntax, A.B, to express selection from the
      // value A and from the type A. We have to try both.
      tryEither(tryCtx => asSelect(tryCtx))((_,_) => asJavaSelectFromTypeTree(ctx))
    } else asSelect(ctx)
  }

  def typedSelectFromTypeTree(tree: untpd.SelectFromTypeTree, pt: Type)(implicit ctx: Context): Tree = track("typedSelectFromTypeTree") {
    val qual1 = typedType(tree.qualifier, selectionProto(tree.name, pt, this))
    checkLegalPrefix(qual1.tpe, tree.name, qual1.pos)
    assignType(cpy.SelectFromTypeTree(tree)(qual1, tree.name), qual1)
  }

  def typedThis(tree: untpd.This)(implicit ctx: Context): Tree = track("typedThis") {
    assignType(tree)
  }

  def typedSuper(tree: untpd.Super, pt: Type)(implicit ctx: Context): Tree = track("typedSuper") {
    val qual1 = typed(tree.qual)
    val inConstrCall = pt match {
      case pt: SelectionProto if pt.name == nme.CONSTRUCTOR => true
      case _ => false
    }
    assignType(cpy.Super(tree)(qual1, tree.mix), qual1, inConstrCall)
  }

  def typedLiteral(tree: untpd.Literal)(implicit ctx: Context) = track("typedLiteral") {
    assignType(tree)
  }

  def typedNew(tree: untpd.New, pt: Type)(implicit ctx: Context) = track("typedNew") {
    tree.tpt match {
      case templ: untpd.Template =>
        import untpd._
        val x = tpnme.ANON_CLASS
        val clsDef = TypeDef(x, templ).withFlags(Final)
        typed(cpy.Block(tree)(clsDef :: Nil, New(Ident(x), Nil)), pt)
      case _ =>
	      val tpt1 = typedType(tree.tpt)
	      checkClassTypeWithStablePrefix(tpt1.tpe, tpt1.pos, traitReq = false)
        assignType(cpy.New(tree)(tpt1), tpt1)
        // todo in a later phase: checkInstantiatable(cls, tpt1.pos)
    }
  }

  def typedPair(tree: untpd.Pair, pt: Type)(implicit ctx: Context) = track("typedPair") {
    val (leftProto, rightProto) = pt.argTypesLo match {
      case l :: r :: Nil if pt isRef defn.PairClass => (l, r)
      case _ => (WildcardType, WildcardType)
    }
    val left1 = typed(tree.left, leftProto)
    val right1 = typed(tree.right, rightProto)
    assignType(cpy.Pair(tree)(left1, right1), left1, right1)
  }

  def typedTyped(tree: untpd.Typed, pt: Type)(implicit ctx: Context): Tree = track("typedTyped") {
    def regularTyped(isWildcard: Boolean) = {
      val tpt1 =
        if (untpd.isWildcardStarArg(tree))
          TypeTree(defn.SeqClass.typeRef.appliedTo(pt :: Nil))
        else
          typedType(tree.tpt)
      val expr1 =
        if (isWildcard) tree.expr withType tpt1.tpe
        else typed(tree.expr, tpt1.tpe)
      assignType(cpy.Typed(tree)(expr1, tpt1), tpt1)
    }
    tree.expr match {
      case id: untpd.Ident if (ctx.mode is Mode.Pattern) && isVarPattern(id) =>
        if (id.name == nme.WILDCARD) regularTyped(isWildcard = true)
        else {
          import untpd._
          typed(Bind(id.name, Typed(Ident(nme.WILDCARD), tree.tpt)).withPos(id.pos), pt)
        }
      case _ =>
        if (untpd.isWildcardStarArg(tree))
          seqToRepeated(typedExpr(tree.expr, defn.SeqType))
        else
          regularTyped(isWildcard = false)
    }
  }

  def typedNamedArg(tree: untpd.NamedArg, pt: Type)(implicit ctx: Context) = track("typedNamedArg") {
    val arg1 = typed(tree.arg, pt)
    assignType(cpy.NamedArg(tree)(tree.name, arg1), arg1)
  }

  def typedAssign(tree: untpd.Assign, pt: Type)(implicit ctx: Context) = track("typedAssign") {
    tree.lhs match {
      case lhs @ Apply(fn, args) =>
        typed(cpy.Apply(lhs)(untpd.Select(fn, nme.update), args :+ tree.rhs), pt)
      case untpd.TypedSplice(Apply(MaybePoly(Select(fn, app), targs), args)) if app == nme.apply =>
        val rawUpdate: untpd.Tree = untpd.Select(untpd.TypedSplice(fn), nme.update)
        val wrappedUpdate = 
          if (targs.isEmpty) rawUpdate 
          else untpd.TypeApply(rawUpdate, targs map untpd.TypedSplice)
        val appliedUpdate = cpy.Apply(fn)(wrappedUpdate, (args map untpd.TypedSplice) :+ tree.rhs)
        typed(appliedUpdate, pt)
      case lhs =>
        val lhsCore = typedUnadapted(lhs)
        def lhs1 = typed(untpd.TypedSplice(lhsCore))
        def canAssign(sym: Symbol) = // allow assignments from the primary constructor to class fields
          sym.is(Mutable, butNot = Accessor) ||
          ctx.owner.isPrimaryConstructor && !sym.is(Method) && sym.owner == ctx.owner.owner
        lhsCore.tpe match {
          case ref: TermRef if canAssign(ref.symbol) =>
            assignType(cpy.Assign(tree)(lhs1, typed(tree.rhs, ref.info)))
          case _ =>
            def reassignmentToVal =
              errorTree(cpy.Assign(tree)(lhsCore, typed(tree.rhs, lhs1.tpe.widen)),
                  "reassignment to val")
            lhsCore.tpe match {
              case ref: TermRef => // todo: further conditions to impose on getter?
                val pre = ref.prefix
                val setterName = ref.name.setterName
                val setter = pre.member(setterName)
                lhsCore match {
                  case lhsCore: RefTree if setter.exists =>
                    val setterTypeRaw = pre select (setterName, setter)
                    val setterType = ensureAccessible(setterTypeRaw, isSuperSelection(lhsCore), tree.pos)
                    val lhs2 = untpd.rename(lhsCore, setterName).withType(setterType)
                    typedUnadapted(cpy.Apply(tree)(untpd.TypedSplice(lhs2), tree.rhs :: Nil))
                  case _ =>
                    reassignmentToVal
                }
              case tpe =>
                reassignmentToVal
            }
        }
    }
  }

  def typedBlock(tree: untpd.Block, pt: Type)(implicit ctx: Context) = track("typedBlock") {
    val exprCtx = index(tree.stats)
    val stats1 = typedStats(tree.stats, ctx.owner)
    val expr1 = typedExpr(tree.expr, pt)(exprCtx)
    ensureNoLocalRefs(assignType(cpy.Block(tree)(stats1, expr1), stats1, expr1), pt)
  }

  def escapingRefs(block: Block)(implicit ctx: Context): collection.Set[NamedType] = {
    var hoisted: Set[Symbol] = Set()
    lazy val locals = localSyms(block.stats).toSet
    def leakingTypes(tp: Type): collection.Set[NamedType] =
      tp namedPartsWith (tp => locals.contains(tp.symbol))
    def typeLeaks(tp: Type): Boolean = leakingTypes(tp).nonEmpty
    def classLeaks(sym: ClassSymbol): Boolean =
      (ctx.owner is Method) || // can't hoist classes out of method bodies
      (sym.info.parents exists typeLeaks) ||
      (sym.info.decls.toList exists (t => typeLeaks(t.info)))
    leakingTypes(block.tpe)
  }

  /** Check that block's type can be expressed without references to locally defined
   *  symbols. The following two remedies are tried before giving up:
   *  1. If the expected type of the block is fully defined, pick it as the
   *     type of the result expressed by adding a type ascription.
   *  2. If (1) fails, force all type variables so that the block's type is
   *     fully defined and try again.
   */
  protected def ensureNoLocalRefs(block: Block, pt: Type, forcedDefined: Boolean = false)(implicit ctx: Context): Tree = {
    val Block(stats, expr) = block
    val leaks = escapingRefs(block)
    if (leaks.isEmpty) block
    else if (isFullyDefined(pt, ForceDegree.none)) {
      val expr1 = Typed(expr, TypeTree(pt))
      cpy.Block(block)(stats, expr1) withType expr1.tpe // no assignType here because avoid is redundant
    } else if (!forcedDefined) {
      fullyDefinedType(block.tpe, "block", block.pos)
      val expr1 = Typed(expr, TypeTree(avoid(block.tpe, localSyms(stats))))
      val block1 = cpy.Block(block)(stats, expr1) withType expr1.tpe // no assignType here because avoid is already done
      ensureNoLocalRefs(block1, pt, forcedDefined = true)
    } else
      errorTree(block,
          d"local definition of ${leaks.head.name} escapes as part of block's type ${block.tpe}"/*; full type: ${result.tpe.toString}"*/)
  }

  def typedIf(tree: untpd.If, pt: Type)(implicit ctx: Context) = track("typedIf") {
    val cond1 = typed(tree.cond, defn.BooleanType)
    val thenp1 = typed(tree.thenp, pt)
    val elsep1 = typed(tree.elsep orElse untpd.unitLiteral withPos tree.pos, pt)
    assignType(cpy.If(tree)(cond1, thenp1, elsep1), thenp1, elsep1)
  }

  def typedFunction(tree: untpd.Function, pt: Type)(implicit ctx: Context) = track("typedFunction") {
    val untpd.Function(args, body) = tree
    if (ctx.mode is Mode.Type)
      typed(cpy.AppliedTypeTree(tree)(
        untpd.TypeTree(defn.FunctionClass(args.length).typeRef), args :+ body), pt)
    else {
      val params = args.asInstanceOf[List[untpd.ValDef]]
      val (protoFormals, protoResult): (List[Type], Type) = pt match {
        case _ if defn.isFunctionType(pt) =>
          (pt.dealias.argInfos.init, pt.dealias.argInfos.last)
        case SAMType(meth) =>
          val mt @ MethodType(_, paramTypes) = meth.info
          (paramTypes, mt.resultType)
        case _ =>
          (params map alwaysWildcardType, WildcardType)
      }

      def refersTo(arg: untpd.Tree, param: untpd.ValDef): Boolean = arg match {
        case Ident(name) => name == param.name
        case _ => false
      }

      /** The funcion body to be returned in the closure. Can become a TypedSplice
       *  of a typed expression if this is necessary to infer a parameter type.
       */
      var fnBody = tree.body


      /** If function is of the form
       *      (x1, ..., xN) => f(x1, ..., XN)
       *  the type of `f`, otherwise NoType. (updates `fnBody` as a side effect).
       */
      def calleeType: Type = fnBody match {
        case Apply(expr, args) if (args corresponds params)(refersTo) =>
          expr match {
            case untpd.TypedSplice(expr1) =>
              expr1.tpe
            case _ =>
              val protoArgs = args map (_ withType WildcardType)
              val callProto = FunProto(protoArgs, WildcardType, this)
              val expr1 = typedExpr(expr, callProto)
              fnBody = cpy.Apply(fnBody)(untpd.TypedSplice(expr1), args)
              expr1.tpe
          }
        case _ =>
          NoType
      }

      /** Two attempts: First, if expected type is fully defined pick this one.
       *  Second, if function is of the form
       *      (x1, ..., xN) => f(x1, ..., XN)
       *  and f has a method type MT, pick the corresponding parameter type in MT,
       *  if this one is fully defined.
       *  If both attempts fail, issue a "missing parameter type" error.
       */
      def inferredParamType(param: untpd.ValDef, formal: Type): Type = {
        if (isFullyDefined(formal, ForceDegree.noBottom)) return formal
        calleeType.widen match {
          case mtpe: MethodType =>
            val pos = params indexWhere (_.name == param.name)
            if (pos < mtpe.paramTypes.length) {
              val ptype = mtpe.paramTypes(pos)
              if (isFullyDefined(ptype, ForceDegree.none)) return ptype
            }
          case _ =>
        }
        val ofFun =
          if (nme.syntheticParamNames(args.length + 1) contains param.name)
            i" of expanded function $tree"
          else
            ""
        errorType(i"missing parameter type for parameter ${param.name}$ofFun, expected = $pt", param.pos)
      }

      def protoFormal(i: Int): Type =
        if (protoFormals.length == params.length) protoFormals(i)
        else errorType(i"wrong number of parameters, expected: ${protoFormals.length}", tree.pos)

      val inferredParams: List[untpd.ValDef] =
        for ((param, i) <- params.zipWithIndex) yield
          if (!param.tpt.isEmpty) param
          else cpy.ValDef(param)(tpt = untpd.TypeTree(inferredParamType(param, protoFormal(i))))

      // Define result type of closure as the expected type, thereby pushing
      // down any implicit searches. We do this even if the expected type is not fully
      // defined, which is a bit of a hack. But it's needed to make the following work
      // (see typers.scala and printers/PlainPrinter.scala for examples).
      //
      //     def double(x: Char): String = s"$x$x"
      //     "abc" flatMap double
      //
      val resultTpt = protoResult match {
        case WildcardType(_) => untpd.TypeTree()
        case _ => untpd.TypeTree(protoResult)
      }
      typed(desugar.makeClosure(inferredParams, fnBody, resultTpt), pt)
    }
  }

  def typedClosure(tree: untpd.Closure, pt: Type)(implicit ctx: Context) = track("typedClosure") {
    val env1 = tree.env mapconserve (typed(_))
    val meth1 = typedUnadapted(tree.meth)
    val target =
      if (tree.tpt.isEmpty)
        meth1.tpe.widen match {
          case mt: MethodType =>
            pt match {
              case SAMType(meth) if !defn.isFunctionType(pt) && mt <:< meth.info =>
                if (!isFullyDefined(pt, ForceDegree.all))
                  ctx.error(d"result type of closure is an underspecified SAM type $pt", tree.pos)
                TypeTree(pt)
              case _ =>
                if (!mt.isDependent) EmptyTree
                else throw new Error(i"internal error: cannot turn dependent method type $mt into closure, position = ${tree.pos}, raw type = ${mt.toString}") // !!! DEBUG. Eventually, convert to an error?
            }
          case tp =>
            throw new Error(i"internal error: closing over non-method $tp, pos = ${tree.pos}")
        }
      else typed(tree.tpt)
    assignType(cpy.Closure(tree)(env1, meth1, target), meth1, target)
  }

  def typedMatch(tree: untpd.Match, pt: Type)(implicit ctx: Context) = track("typedMatch") {
    tree.selector match {
      case EmptyTree =>
        typed(desugar.makeCaseLambda(tree.cases) withPos tree.pos, pt)
      case _ =>
        val sel1 = typedExpr(tree.selector)
        val selType = widenForMatchSelector(
            fullyDefinedType(sel1.tpe, "pattern selector", tree.pos))

        val cases1 = typedCases(tree.cases, selType, pt)
        assignType(cpy.Match(tree)(sel1, cases1), cases1)
    }
  }

  def typedCases(cases: List[untpd.CaseDef], selType: Type, pt: Type)(implicit ctx: Context) = {

    /** gadtSyms = "all type parameters of enclosing methods that appear
     *              non-variantly in the selector type" todo: should typevars
     *              which appear with variances +1 and -1 (in different
     *              places) be considered as well?
     */
    val gadtSyms: Set[Symbol] = ctx.traceIndented(i"GADT syms of $selType", gadts) {
      val accu = new TypeAccumulator[Set[Symbol]] {
        def apply(tsyms: Set[Symbol], t: Type): Set[Symbol] = {
          val tsyms1 = t match {
            case tr: TypeRef if (tr.symbol is TypeParam) && tr.symbol.owner.isTerm && variance == 0 =>
              tsyms + tr.symbol
            case _ =>
              tsyms
          }
          foldOver(tsyms1, t)
        }
      }
      accu(Set.empty, selType)
    }

    cases mapconserve (typedCase(_, pt, selType, gadtSyms))
  }

  /** Type a case. Overridden in ReTyper, that's why it's separate from
   *  typedCases.
   */
  def typedCase(tree: untpd.CaseDef, pt: Type, selType: Type, gadtSyms: Set[Symbol])(implicit ctx: Context): CaseDef = track("typedCase") {
    val originalCtx = ctx

    def caseRest(pat: Tree)(implicit ctx: Context) = {
      pat foreachSubTree {
        case b: Bind =>
          if (ctx.scope.lookup(b.name) == NoSymbol) ctx.enter(b.symbol)
          else ctx.error(d"duplicate pattern variable: ${b.name}", b.pos)
        case _ =>
      }
      val guard1 = typedExpr(tree.guard, defn.BooleanType)
      val body1 = typedExpr(tree.body, pt)
        .ensureConforms(pt)(originalCtx) // insert a cast if body does not conform to expected type if we disregard gadt bounds
      assignType(cpy.CaseDef(tree)(pat, guard1, body1), body1)
    }

    val gadtCtx =
      if (gadtSyms.isEmpty) ctx
      else {
        val c = ctx.fresh.setFreshGADTBounds
        for (sym <- gadtSyms)
          if (!c.gadt.bounds.contains(sym))
            c.gadt.setBounds(sym, TypeBounds.empty)
        c
      }
    val pat1 = typedPattern(tree.pat, selType)(gadtCtx)
    caseRest(pat1)(gadtCtx.fresh.setNewScope)
  }

  def typedReturn(tree: untpd.Return)(implicit ctx: Context): Return = track("typedReturn") {
    def returnProto(owner: Symbol, locals: Scope): Type =
      if (owner.isConstructor) defn.UnitType
      else owner.info match {
        case info: PolyType =>
          val tparams = locals.toList.takeWhile(_ is TypeParam)
          assert(info.paramNames.length == tparams.length,
                 i"return mismatch from $owner, tparams = $tparams, locals = ${locals.toList}%, %")
          info.instantiate(tparams.map(_.typeRef)).finalResultType
        case info =>
          info.finalResultType
      }
    def enclMethInfo(cx: Context): (Tree, Type) = {
      val owner = cx.owner
      if (cx == NoContext || owner.isType) {
        ctx.error("return outside method definition", tree.pos)
        (EmptyTree, WildcardType)
      }
      else if (owner != cx.outer.owner && owner.isSourceMethod) {
        if (owner.isCompleted) {
          val from = Ident(TermRef(NoPrefix, owner.asTerm))
          val proto = returnProto(owner, cx.scope)
          (from, proto)
        }
        else (EmptyTree, errorType(d"$owner has return statement; needs result type", tree.pos))
      }
      else enclMethInfo(cx.outer)
    }
    val (from, proto) =
      if (tree.from.isEmpty) enclMethInfo(ctx)
      else {
        val from = tree.from.asInstanceOf[tpd.Tree]
        val proto = 
          if (ctx.erasedTypes) from.symbol.info.finalResultType 
          else WildcardType // We cannot reliably detect the internal type view of polymorphic or dependent methods
                            // because we do not know the internal type params and method params.
                            // Hence no adaptation is possible, and we assume WildcardType as prototype.
        (from, proto)
      }
    val expr1 = typedExpr(tree.expr orElse untpd.unitLiteral.withPos(tree.pos), proto)
    assignType(cpy.Return(tree)(expr1, from))
  }

  def typedTry(tree: untpd.Try, pt: Type)(implicit ctx: Context): Try = track("typedTry") {
    val expr1 = typed(tree.expr, pt)
    val cases1 = typedCases(tree.cases, defn.ThrowableType, pt)
    val finalizer1 = typed(tree.finalizer, defn.UnitType)
    assignType(cpy.Try(tree)(expr1, cases1, finalizer1), expr1, cases1)
  }

  def typedThrow(tree: untpd.Throw)(implicit ctx: Context): Tree = track("typedThrow") {
    val expr1 = typed(tree.expr, defn.ThrowableType)
    Throw(expr1).withPos(tree.pos)
  }

  def typedSeqLiteral(tree: untpd.SeqLiteral, pt: Type)(implicit ctx: Context): SeqLiteral = track("typedSeqLiteral") {
    val proto1 = pt.elemType orElse WildcardType
    val elems1 = tree.elems mapconserve (typed(_, proto1))
    assignType(cpy.SeqLiteral(tree)(elems1), elems1)
  }

  def typedTypeTree(tree: untpd.TypeTree, pt: Type)(implicit ctx: Context): TypeTree = track("typedTypeTree") {
    if (tree.original.isEmpty)
      tree match {
        case tree: untpd.DerivedTypeTree =>
          tree.ensureCompletions
          try
            TypeTree(tree.derivedType(tree.attachment(untpd.OriginalSymbol))) withPos tree.pos
            // btw, no need to remove the attachment. The typed
            // tree is different from the untyped one, so the
            // untyped tree is no longer accessed after all
            // accesses with typedTypeTree are done.
          catch {
            case ex: NoSuchElementException =>
              println(s"missing OriginalSymbol for ${ctx.owner.ownersIterator.toList}")
              throw ex
          }
        case _ =>
          assert(isFullyDefined(pt, ForceDegree.none))
          tree.withType(pt)
      }
    else {
      val original1 = typed(tree.original)
      cpy.TypeTree(tree)(original1).withType(original1.tpe)
    }
  }

  def typedSingletonTypeTree(tree: untpd.SingletonTypeTree)(implicit ctx: Context): SingletonTypeTree = track("typedSingletonTypeTree") {
    val ref1 = typedExpr(tree.ref)
    checkStable(ref1.tpe, tree.pos)
    assignType(cpy.SingletonTypeTree(tree)(ref1), ref1)
  }

  def typedAndTypeTree(tree: untpd.AndTypeTree)(implicit ctx: Context): AndTypeTree = track("typedAndTypeTree") {
    val left1 = typed(tree.left)
    val right1 = typed(tree.right)
    assignType(cpy.AndTypeTree(tree)(left1, right1), left1, right1)
  }

  def typedOrTypeTree(tree: untpd.OrTypeTree)(implicit ctx: Context): OrTypeTree = track("typedOrTypeTree") {
    val left1 = typed(tree.left)
    val right1 = typed(tree.right)
    assignType(cpy.OrTypeTree(tree)(left1, right1), left1, right1)
  }

  def typedRefinedTypeTree(tree: untpd.RefinedTypeTree)(implicit ctx: Context): RefinedTypeTree = track("typedRefinedTypeTree") {
    val tpt1 = if (tree.tpt.isEmpty) TypeTree(defn.ObjectType) else typedAheadType(tree.tpt)
    val refineClsDef = desugar.refinedTypeToClass(tpt1, tree.refinements)
    val refineCls = createSymbol(refineClsDef).asClass
    val TypeDef(_, Template(_, _, _, refinements1)) = typed(refineClsDef)
    val seen = mutable.Set[Symbol]()
    assert(tree.refinements.length == refinements1.length, s"${tree.refinements} != $refinements1")
    def addRefinement(parent: Type, refinement: Tree): Type = {
      typr.println(s"adding refinement $refinement")
      checkRefinementNonCyclic(refinement, refineCls, seen)
      val rsym = refinement.symbol
      val rinfo = if (rsym is Accessor) rsym.info.resultType else rsym.info
      RefinedType(parent, rsym.name, rt => rinfo.substThis(refineCls, SkolemType(rt)))
      // todo later: check that refinement is within bounds
    }
    val res = cpy.RefinedTypeTree(tree)(tpt1, refinements1) withType
      (tpt1.tpe /: refinements1)(addRefinement)
    typr.println(i"typed refinement: ${res.tpe}")
    res
  }

  def typedAppliedTypeTree(tree: untpd.AppliedTypeTree)(implicit ctx: Context): AppliedTypeTree = track("typedAppliedTypeTree") {
    val tpt1 = typed(tree.tpt)
    val argPts =
      if (ctx.mode is Mode.Pattern) tpt1.tpe.typeParams.map(_.info)
      else tree.args.map(_ => WildcardType)
    val args1 = tree.args.zipWithConserve(argPts)(typed(_, _)).asInstanceOf[List[Tree]]
    // check that arguments conform to bounds is done in phase FirstTransform
    assignType(cpy.AppliedTypeTree(tree)(tpt1, args1), tpt1, args1)
  }

  def typedByNameTypeTree(tree: untpd.ByNameTypeTree)(implicit ctx: Context): ByNameTypeTree = track("typedByNameTypeTree") {
    val result1 = typed(tree.result)
    assignType(cpy.ByNameTypeTree(tree)(result1), result1)
  }

  def typedTypeBoundsTree(tree: untpd.TypeBoundsTree)(implicit ctx: Context): TypeBoundsTree = track("typedTypeBoundsTree") {
    val TypeBoundsTree(lo, hi) = desugar.typeBoundsTree(tree)
    val lo1 = typed(lo)
    val hi1 = typed(hi)
    if (!(lo1.tpe <:< hi1.tpe))
      ctx.error(d"lower bound ${lo1.tpe} does not conform to upper bound ${hi1.tpe}", tree.pos)
    assignType(cpy.TypeBoundsTree(tree)(lo1, hi1), lo1, hi1)
  }

  def typedBind(tree: untpd.Bind, pt: Type)(implicit ctx: Context): Bind = track("typedBind") {
    val body1 = typed(tree.body, pt)
    typr.println(i"typed bind $tree pt = $pt bodytpe = ${body1.tpe}")
    val sym = ctx.newSymbol(ctx.owner, tree.name, EmptyFlags, body1.tpe, coord = tree.pos)
    assignType(cpy.Bind(tree)(tree.name, body1), sym)
  }

  def typedAlternative(tree: untpd.Alternative, pt: Type)(implicit ctx: Context): Alternative = track("typedAlternative") {
    val trees1 = tree.trees mapconserve (typed(_, pt))
    assignType(cpy.Alternative(tree)(trees1), trees1)
  }

  def addTypedModifiersAnnotations(mdef: untpd.MemberDef, sym: Symbol)(implicit ctx: Context): Unit = {
    val mods1 = typedModifiers(untpd.modsDeco(mdef).mods, sym)
    for (tree <- mods1.annotations) sym.addAnnotation(Annotation(tree))
  }

  def typedModifiers(mods: untpd.Modifiers, sym: Symbol)(implicit ctx: Context): Modifiers = track("typedModifiers") {
    val annotations1 = mods.annotations mapconserve typedAnnotation
    if (annotations1 eq mods.annotations) mods.asInstanceOf[Modifiers]
    else Modifiers(mods.flags, mods.privateWithin, annotations1)
  }

  def typedAnnotation(annot: untpd.Tree)(implicit ctx: Context): Tree = track("typedAnnotation") {
    typed(annot, defn.AnnotationClass.typeRef)
  }

  def typedValDef(vdef: untpd.ValDef, sym: Symbol)(implicit ctx: Context) = track("typedValDef") {
    val ValDef(name, tpt, rhs) = vdef
    addTypedModifiersAnnotations(vdef, sym)
    val tpt1 = typedType(tpt)
    val rhs1 = rhs match {
      case Ident(nme.WILDCARD) => rhs withType tpt1.tpe
      case _ => typedExpr(rhs, tpt1.tpe)
    }
    assignType(cpy.ValDef(vdef)(name, tpt1, rhs1), sym)
  }

  def typedDefDef(ddef: untpd.DefDef, sym: Symbol)(implicit ctx: Context) = track("typedDefDef") {
    val DefDef(name, tparams, vparamss, tpt, rhs) = ddef
    addTypedModifiersAnnotations(ddef, sym)
    val tparams1 = tparams mapconserve (typed(_).asInstanceOf[TypeDef])
    val vparamss1 = vparamss nestedMapconserve (typed(_).asInstanceOf[ValDef])
    if (sym is Implicit) checkImplicitParamsNotSingletons(vparamss1)
    val tpt1 = typedType(tpt)
    val rhs1 = typedExpr(rhs, tpt1.tpe)
    assignType(cpy.DefDef(ddef)(name, tparams1, vparamss1, tpt1, rhs1), sym)
    //todo: make sure dependent method types do not depend on implicits or by-name params
  }

  def typedTypeDef(tdef: untpd.TypeDef, sym: Symbol)(implicit ctx: Context): Tree = track("typedTypeDef") {
    val TypeDef(name, rhs) = tdef
    addTypedModifiersAnnotations(tdef, sym)
    val _ = typedType(rhs) // unused, typecheck only to remove from typedTree
    assignType(cpy.TypeDef(tdef)(name, TypeTree(sym.info), Nil), sym)
  }

  def typedClassDef(cdef: untpd.TypeDef, cls: ClassSymbol)(implicit ctx: Context) = track("typedClassDef") {
    val TypeDef(name, impl @ Template(constr, parents, self, body)) = cdef
    val superCtx = ctx.superCallContext
    def typedParent(tree: untpd.Tree): Tree =
      if (tree.isType) typedType(tree)(superCtx)
      else {
        val result = typedExpr(tree)(superCtx)
        if ((cls is Trait) && result.tpe.classSymbol.isRealClass && !ctx.isAfterTyper)
          ctx.error(s"trait may not call constructor of ${result.tpe.classSymbol}", tree.pos)
        result
      }

    addTypedModifiersAnnotations(cdef, cls)
    val constr1 = typed(constr).asInstanceOf[DefDef]
    val parentsWithClass = ensureFirstIsClass(parents mapconserve typedParent, cdef.pos.toSynthetic)
    val parents1 = ensureConstrCall(cls, parentsWithClass)(superCtx)
    val self1 = typed(self)(ctx.outer).asInstanceOf[ValDef] // outer context where class members are not visible
    val dummy = localDummy(cls, impl)
    val body1 = typedStats(body, dummy)(inClassContext(self1.symbol))
    checkNoDoubleDefs(cls)
    val impl1 = cpy.Template(impl)(constr1, parents1, self1, body1)
      .withType(dummy.nonMemberTermRef)
    checkVariance(impl1)
    assignType(cpy.TypeDef(cdef)(name, impl1, Nil), cls)

    // todo later: check that
    //  1. If class is non-abstract, it is instantiatable:
    //  - self type is s supertype of own type
    //  - all type members have consistent bounds
    // 2. all private type members have consistent bounds
    // 3. Types do not override classes.
    // 4. Polymorphic type defs override nothing.
  }

  /** If this is a real class, make sure its first parent is a
   *  constructor call. Cannot simply use a type. Overridden in ReTyper.
   */
  def ensureConstrCall(cls: ClassSymbol, parents: List[Tree])(implicit ctx: Context): List[Tree] = {
    val firstParent :: otherParents = parents
    if (firstParent.isType && !(cls is Trait))
      typed(untpd.New(untpd.TypedSplice(firstParent), Nil)) :: otherParents
    else parents
  }

  /** Overridden in retyper */
  def checkVariance(tree: Tree)(implicit ctx: Context) = VarianceChecker.check(tree)

  def localDummy(cls: ClassSymbol, impl: untpd.Template)(implicit ctx: Context): Symbol =
    ctx.newLocalDummy(cls, impl.pos)

  def typedImport(imp: untpd.Import, sym: Symbol)(implicit ctx: Context): Import = track("typedImport") {
    val expr1 = typedExpr(imp.expr, AnySelectionProto)
    checkStable(expr1.tpe, imp.expr.pos)
    assignType(cpy.Import(imp)(expr1, imp.selectors), sym)
  }

  def typedPackageDef(tree: untpd.PackageDef)(implicit ctx: Context): Tree = track("typedPackageDef") {
    val pid1 = typedExpr(tree.pid, AnySelectionProto)
    val pkg = pid1.symbol
    val packageContext =
      if (pkg is Package) ctx.fresh.setOwner(pkg.moduleClass).setTree(tree)
      else {
        ctx.error(d"$pkg is not a packge", tree.pos)
        ctx
      }
    val stats1 = typedStats(tree.stats, pkg.moduleClass)(packageContext)
    cpy.PackageDef(tree)(pid1.asInstanceOf[RefTree], stats1) withType pkg.valRef
  }

  def typedAnnotated(tree: untpd.Annotated, pt: Type)(implicit ctx: Context): Tree = track("typedAnnotated") {
    val annot1 = typedExpr(tree.annot, defn.AnnotationClass.typeRef)
    val arg1 = typed(tree.arg, pt)
    if (ctx.mode is Mode.Type)
      assignType(cpy.Annotated(tree)(annot1, arg1), annot1, arg1)
    else {
      val tpt = TypeTree(AnnotatedType(Annotation(annot1), arg1.tpe.widen))
      assignType(cpy.Typed(tree)(arg1, tpt), tpt)
    }
  }

  def typedAsFunction(tree: untpd.Tree, pt: Type)(implicit ctx: Context): Tree =
    typed(tree, if (defn.isFunctionType(pt)) pt else AnyFunctionProto)

  /** Retrieve symbol attached to given tree */
  protected def retrieveSym(tree: untpd.Tree)(implicit ctx: Context) = tree.removeAttachment(SymOfTree) match {
    case Some(sym) =>
      sym.ensureCompleted()
      sym
    case none =>
      NoSymbol
  }

  /** A fresh local context with given tree and owner.
   *  Owner might not exist (can happen for self valdefs), in which case
   *  no owner is set in result context
   */
  protected def localContext(tree: untpd.Tree, owner: Symbol)(implicit ctx: Context): FreshContext = {
    val freshCtx = ctx.fresh.setTree(tree)
    if (owner.exists) freshCtx.setOwner(owner) else freshCtx
  }

  protected def localTyper(sym: Symbol): Typer = nestedTyper.remove(sym).get

  def typedUnadapted(initTree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree = {
    record("typedUnadapted")
    val xtree = expanded(initTree)
    xtree.removeAttachment(TypedAhead) match {
      case Some(ttree) => ttree
      case none =>

        def typedNamed(tree: untpd.NameTree, pt: Type)(implicit ctx: Context): Tree = {
          val sym = retrieveSym(xtree)
          tree match {
            case tree: untpd.Ident => typedIdent(tree, pt)
            case tree: untpd.Select => typedSelect(tree, pt)
            case tree: untpd.SelectFromTypeTree => typedSelectFromTypeTree(tree, pt)
            case tree: untpd.Bind => typedBind(tree, pt)
            case tree: untpd.ValDef =>
              if (tree.isEmpty) tpd.EmptyValDef
              else typedValDef(tree, sym)(localContext(tree, sym).setNewScope)
            case tree: untpd.DefDef =>
              val typer1 = localTyper(sym)
              typer1.typedDefDef(tree, sym)(localContext(tree, sym).setTyper(typer1))
            case tree: untpd.TypeDef =>
              if (tree.isClassDef)
                typedClassDef(tree, sym.asClass)(localContext(tree, sym).setMode(ctx.mode &~ Mode.InSuperCall))
              else
                typedTypeDef(tree, sym)(localContext(tree, sym).setNewScope)
            case _ => typedUnadapted(desugar(tree), pt)
          }
        }

        def typedUnnamed(tree: untpd.Tree): Tree = tree match {
          case tree: untpd.Apply =>
            if (ctx.mode is Mode.Pattern) typedUnApply(tree, pt) else typedApply(tree, pt)
          case tree: untpd.This => typedThis(tree)
          case tree: untpd.Literal => typedLiteral(tree)
          case tree: untpd.New => typedNew(tree, pt)
          case tree: untpd.Pair => typedPair(tree, pt)
          case tree: untpd.Typed => typedTyped(tree, pt)
          case tree: untpd.NamedArg => typedNamedArg(tree, pt)
          case tree: untpd.Assign => typedAssign(tree, pt)
          case tree: untpd.Block => typedBlock(desugar.block(tree), pt)(ctx.fresh.setNewScope)
          case tree: untpd.If => typedIf(tree, pt)
          case tree: untpd.Function => typedFunction(tree, pt)
          case tree: untpd.Closure => typedClosure(tree, pt)
          case tree: untpd.Match => typedMatch(tree, pt)
          case tree: untpd.Return => typedReturn(tree)
          case tree: untpd.Try => typedTry(tree, pt)
          case tree: untpd.Throw => typedThrow(tree)
          case tree: untpd.TypeApply => typedTypeApply(tree, pt)
          case tree: untpd.Super => typedSuper(tree, pt)
          case tree: untpd.SeqLiteral => typedSeqLiteral(tree, pt)
          case tree: untpd.TypeTree => typedTypeTree(tree, pt)
          case tree: untpd.SingletonTypeTree => typedSingletonTypeTree(tree)
          case tree: untpd.AndTypeTree => typedAndTypeTree(tree)
          case tree: untpd.OrTypeTree => typedOrTypeTree(tree)
          case tree: untpd.RefinedTypeTree => typedRefinedTypeTree(tree)
          case tree: untpd.AppliedTypeTree => typedAppliedTypeTree(tree)
          case tree: untpd.ByNameTypeTree => typedByNameTypeTree(tree)
          case tree: untpd.TypeBoundsTree => typedTypeBoundsTree(tree)
          case tree: untpd.Alternative => typedAlternative(tree, pt)
          case tree: untpd.PackageDef => typedPackageDef(tree)
          case tree: untpd.Annotated => typedAnnotated(tree, pt)
          case tree: untpd.TypedSplice => tree.tree
          case tree:  untpd.UnApply => typedUnApply(tree, pt)
          case untpd.PostfixOp(tree, nme.WILDCARD) => typedAsFunction(tree, pt)
          case untpd.EmptyTree => tpd.EmptyTree
          case _ => typedUnadapted(desugar(tree), pt)
        }

        xtree match {
          case xtree: untpd.NameTree => typedNamed(encodeName(xtree), pt)
          case xtree: untpd.Import => typedImport(xtree, retrieveSym(xtree))
          case xtree => typedUnnamed(xtree)
        }
    }
  }

  protected def encodeName(tree: untpd.NameTree)(implicit ctx: Context): untpd.NameTree =
    untpd.rename(tree, tree.name.encode)

  def typed(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree = /*>|>*/ ctx.traceIndented (i"typing $tree", typr, show = true) /*<|<*/ {
    assertPositioned(tree)
    try adapt(typedUnadapted(tree, pt), pt, tree)
    catch {
      case ex: CyclicReference => errorTree(tree, cyclicErrorMsg(ex))
      case ex: FatalTypeError => errorTree(tree, ex.getMessage)
    }
  }

  def typedTrees(trees: List[untpd.Tree])(implicit ctx: Context): List[Tree] =
    trees mapconserve (typed(_))

  def typedStats(stats: List[untpd.Tree], exprOwner: Symbol)(implicit ctx: Context): List[tpd.Tree] = {
    val buf = new mutable.ListBuffer[Tree]
    @tailrec def traverse(stats: List[untpd.Tree])(implicit ctx: Context): List[Tree] = stats match {
      case (imp: untpd.Import) :: rest =>
        val imp1 = typed(imp)
        buf += imp1
        traverse(rest)(importContext(imp1.symbol, imp.selectors))
      case (mdef: untpd.DefTree) :: rest =>
        mdef.removeAttachment(ExpandedTree) match {
          case Some(xtree) =>
            traverse(xtree :: rest)
          case none =>
            buf += typed(mdef)
            traverse(rest)
        }
      case Thicket(stats) :: rest =>
        traverse(stats ++ rest)
      case stat :: rest =>
        buf += typed(stat)(ctx.exprContext(stat, exprOwner))
        traverse(rest)
      case nil =>
        buf.toList
    }
    traverse(stats)
  }

  def typedExpr(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree =
    typed(tree, pt)(ctx retractMode Mode.PatternOrType)
  def typedType(tree: untpd.Tree, pt: Type = WildcardType)(implicit ctx: Context): Tree = // todo: retract mode between Type and Pattern?
    typed(tree, pt)(ctx addMode Mode.Type)
  def typedPattern(tree: untpd.Tree, selType: Type = WildcardType)(implicit ctx: Context): Tree =
    typed(tree, selType)(ctx addMode Mode.Pattern)

  def tryEither[T](op: Context => T)(fallBack: (T, TyperState) => T)(implicit ctx: Context) = {
    val nestedCtx = ctx.fresh.setNewTyperState
    val result = op(nestedCtx)
    if (nestedCtx.reporter.hasErrors)
      fallBack(result, nestedCtx.typerState)
    else {
      nestedCtx.typerState.commit()
      result
    }
  }

  /** Add apply node or implicit conversions. Two strategies are tried, and the first
   *  that is succesful is picked. If neither of the strategies are succesful, continues with
   *  `fallBack`.
   *
   *  1st strategy: Try to insert `.apply` so that the result conforms to prototype `pt`.
   *  2nd stratgey: If tree is a select `qual.name`, try to insert an implicit conversion
   *    around the qualifier part `qual` so that the result conforms to the expected type
   *    with wildcard result type.
   */
  def tryInsertApplyOrImplicit(tree: Tree, pt: ProtoType)(fallBack: (Tree, TyperState) => Tree)(implicit ctx: Context): Tree =
    tryEither { implicit ctx =>
      val sel = typedSelect(untpd.Select(untpd.TypedSplice(tree), nme.apply), pt)
      if (sel.tpe.isError) sel else adapt(sel, pt)
    } { (failedTree, failedState) =>
      val tree1 = tryInsertImplicitOnQualifier(tree, pt)
      if (tree1 eq tree) fallBack(failedTree, failedState)
      else adapt(tree1, pt)
    }

  /** If this tree is a select node `qual.name`, try to insert an implicit conversion
   *  `c` around `qual` so that `c(qual).name` conforms to `pt`. If that fails
   *  return `tree` itself.
   */
  def tryInsertImplicitOnQualifier(tree: Tree, pt: Type)(implicit ctx: Context): Tree = ctx.traceIndented(i"try insert impl on qualifier $tree $pt") {
    tree match {
      case Select(qual, name) =>
        val qualProto = SelectionProto(name, pt, NoViewsAllowed)
        tryEither { implicit ctx =>
          val qual1 = adaptInterpolated(qual, qualProto, EmptyTree)
          if ((qual eq qual1) || ctx.reporter.hasErrors) tree
          else typedSelect(cpy.Select(tree)(untpd.TypedSplice(qual1), name), pt)
        } { (_, _) => tree
        }
      case _ => tree
    }
  }

  def adapt(tree: Tree, pt: Type, original: untpd.Tree = untpd.EmptyTree)(implicit ctx: Context) = /*>|>*/ track("adapt") /*<|<*/ {
    /*>|>*/ ctx.traceIndented(i"adapting $tree of type ${tree.tpe} to $pt", typr, show = true) /*<|<*/ {
      interpolateUndetVars(tree)
      tree overwriteType tree.tpe.simplified
      adaptInterpolated(tree, pt, original)
    }
  }

  /** (-1) For expressions with annotated types, let AnnotationCheckers decide what to do
   *  (0) Convert expressions with constant types to literals (unless in interactive/scaladoc mode)
   */

  /** Perform the following adaptations of expression, pattern or type `tree` wrt to
   *  given prototype `pt`:
   *  (1) Resolve overloading
   *  (2) Apply parameterless functions
   *  (3) Apply polymorphic types to fresh instances of their type parameters and
   *      store these instances in context.undetparams,
   *      unless followed by explicit type application.
   *  (4) Do the following to unapplied methods used as values:
   *  (4.1) If the method has only implicit parameters pass implicit arguments
   *  (4.2) otherwise, if `pt` is a function type and method is not a constructor,
   *        convert to function by eta-expansion,
   *  (4.3) otherwise, if the method is nullary with a result type compatible to `pt`
   *        and it is not a constructor, apply it to ()
   *  otherwise issue an error
   *  (5) Convert constructors in a pattern as follows:
   *  (5.1) If constructor refers to a case class factory, set tree's type to the unique
   *        instance of its primary constructor that is a subtype of the expected type.
   *  (5.2) If constructor refers to an extractor, convert to application of
   *        unapply or unapplySeq method.
   *
   *  (6) Convert all other types to TypeTree nodes.
   *  (7) When in TYPEmode but not FUNmode or HKmode, check that types are fully parameterized
   *      (7.1) In HKmode, higher-kinded types are allowed, but they must have the expected kind-arity
   *  (8) When in both EXPRmode and FUNmode, add apply method calls to values of object type.
   *  (9) If there are undetermined type variables and not POLYmode, infer expression instance
   *  Then, if tree's type is not a subtype of expected type, try the following adaptations:
   *  (10) If the expected type is Byte, Short or Char, and the expression
   *      is an integer fitting in the range of that type, convert it to that type.
   *  (11) Widen numeric literals to their expected type, if necessary
   *  (12) When in mode EXPRmode, convert E to { E; () } if expected type is scala.Unit.
   *  (13) When in mode EXPRmode, apply AnnotationChecker conversion if expected type is annotated.
   *  (14) When in mode EXPRmode, apply a view
   *  If all this fails, error
   */
  def adaptInterpolated(tree: Tree, pt: Type, original: untpd.Tree)(implicit ctx: Context): Tree = {

    assert(pt.exists)

    def methodStr = err.refStr(methPart(tree).tpe)

    def adaptOverloaded(ref: TermRef) = {
      val altDenots = ref.denot.alternatives
      typr.println(i"adapt overloaded $ref with alternatives ${altDenots map (_.info)}%, %")
      val alts = altDenots map (alt =>
        TermRef.withSigAndDenot(ref.prefix, ref.name, alt.info.signature, alt))
      def expectedStr = err.expectedTypeStr(pt)
      resolveOverloaded(alts, pt) match {
        case alt :: Nil =>
          adapt(tree.withType(alt), pt, original)
        case Nil =>
          def noMatches =
            errorTree(tree,
              d"""none of the ${err.overloadedAltsStr(altDenots)}
                 |match $expectedStr""".stripMargin)
          def hasEmptyParams(denot: SingleDenotation) = denot.info.paramTypess == ListOfNil
          pt match {
            case pt: FunProto =>
              tryInsertApplyOrImplicit(tree, pt)((_, _) => noMatches)
            case _ =>
              if (altDenots exists (_.info.paramTypess == ListOfNil))
                typed(untpd.Apply(untpd.TypedSplice(tree), Nil), pt)
              else
                noMatches
          }
        case alts =>
          val remainingDenots = alts map (_.denot.asInstanceOf[SingleDenotation])
          def all = if (remainingDenots.length == 2) "both" else "all"
          errorTree(tree,
            d"""Ambiguous overload. The ${err.overloadedAltsStr(remainingDenots)}
               |$all match $expectedStr""".stripMargin)
      }
    }

    def adaptToArgs(wtp: Type, pt: FunProto): Tree = wtp match {
      case _: MethodType | _: PolyType =>
        def isUnary = wtp.firstParamTypes match {
          case ptype :: Nil => !ptype.isRepeatedParam
          case _ => false
        }
        if (pt.args.lengthCompare(1) > 0 && isUnary && ctx.canAutoTuple)
          adaptToArgs(wtp, pt.tupled)
        else
          tree
      case _ => tryInsertApplyOrImplicit(tree, pt) {
        val more = tree match {
          case Apply(_, _) => " more"
          case _ => ""
        }
        (_, _) => errorTree(tree, d"$methodStr does not take$more parameters")
      }
    }

    def adaptNoArgs(wtp: Type): Tree = wtp match {
      case wtp: ExprType =>
        adaptInterpolated(tree.withType(wtp.resultType), pt, original)
      case wtp: ImplicitMethodType if constrainResult(wtp, pt) =>
        def addImplicitArgs = {
          def implicitArgError(msg: => String): Tree = {
            ctx.error(msg, tree.pos.endPos)
            EmptyTree
          }
          val args = (wtp.paramNames, wtp.paramTypes).zipped map { (pname, formal) =>
            def where = d"parameter $pname of $methodStr"
            inferImplicit(formal, EmptyTree, tree.pos.endPos) match {
              case SearchSuccess(arg, _, _) =>
                adapt(arg, formal)
              case ambi: AmbiguousImplicits =>
                implicitArgError(s"ambiguous implicits: ${ambi.explanation} of $where")
              case failure: SearchFailure =>
                implicitArgError(d"no implicit argument of type $formal found for $where" + failure.postscript)
            }
          }
          adapt(tpd.Apply(tree, args), pt)
        }
        if ((pt eq WildcardType) || original.isEmpty) addImplicitArgs
        else
          ctx.typerState.tryWithFallback(addImplicitArgs) {
            adapt(typed(original, WildcardType), pt, EmptyTree)
          }
      case wtp: MethodType if !pt.isInstanceOf[SingletonType] =>
        val arity =
          if (defn.isFunctionType(pt)) defn.functionArity(pt)
          else if (pt eq AnyFunctionProto) wtp.paramTypes.length
          else -1
        if (arity >= 0 && !tree.symbol.isConstructor)
          typed(etaExpand(tree, wtp, arity), pt)
        else if (wtp.paramTypes.isEmpty)
          adaptInterpolated(tpd.Apply(tree, Nil), pt, EmptyTree)
        else if (wtp.isImplicit)
          err.typeMismatch(tree, pt)
        else
          errorTree(tree,
            d"""missing arguments for $methodStr
               |follow this method with `_' if you want to treat it as a partially applied function""".stripMargin)
      case _ =>
        if (tree.tpe <:< pt) tree
        else if (ctx.mode is Mode.Pattern) tree // no subtype check for pattern
        else {
          typr.println(i"adapt to subtype ${tree.tpe} !<:< $pt")
          //typr.println(TypeComparer.explained(implicit ctx => tree.tpe <:< pt))
          adaptToSubType(wtp)
        }
    }

    def adaptToSubType(wtp: Type): Tree = {
      // try converting a constant to the target type
      val folded = ConstFold(tree, pt)
      if (folded ne tree) return folded
      // drop type if prototype is Unit
      if (pt isRef defn.UnitClass)
        return tpd.Block(tree :: Nil, Literal(Constant(())))
      // convert function literal to SAM closure
      tree match {
        case Closure(Nil, id @ Ident(nme.ANON_FUN), _)
        if defn.isFunctionType(wtp) && !defn.isFunctionType(pt) =>
          pt match {
            case SAMType(meth)
            if wtp <:< meth.info.toFunctionType() =>
              // was ... && isFullyDefined(pt, ForceDegree.noBottom)
              // but this prevents case blocks from implementing polymorphic partial functions,
              // since we do not know the result parameter a priori. Have to wait until the
              // body is typechecked.
              return cpy.Closure(tree)(Nil, id, TypeTree(pt)).withType(pt)
            case _ =>
          }
        case _ =>
      }
      // try an implicit conversion
      inferView(tree, pt) match {
        case SearchSuccess(inferred, _, _) =>
          adapt(inferred, pt)
        case failure: SearchFailure =>
          if (pt.isInstanceOf[ProtoType]) tree
          else err.typeMismatch(tree, pt, failure)
      }
    }

    tree match {
      case _: MemberDef | _: PackageDef | _: Import | _: WithoutTypeOrPos[_] => tree
      case _ => tree.tpe.widen match {
        case ErrorType =>
          tree
        case ref: TermRef =>
          adaptOverloaded(ref)
        case poly: PolyType =>
          if (pt.isInstanceOf[PolyProto]) tree
          else {
            var typeArgs = tree match {
              case Select(New(tpt), nme.CONSTRUCTOR) => tpt.tpe.dealias.argTypesLo
              case _ => Nil
            }
            if (typeArgs.isEmpty) typeArgs = constrained(poly, tree)._2
            convertNewArray(
              adaptInterpolated(tree.appliedToTypes(typeArgs), pt, original))
          }
        case wtp =>
          pt match {
            case pt: FunProto =>
              adaptToArgs(wtp, pt)
            case pt: PolyProto =>
              tryInsertApplyOrImplicit(tree, pt) {
                (_, _) => tree // error will be reported in typedTypeApply
              }
            case _ =>
              if (ctx.mode is Mode.Type)
                if ((ctx.mode is Mode.Pattern) || tree.tpe <:< pt) tree
                else err.typeMismatch(tree, pt)
              else adaptNoArgs(wtp)
          }
      }
    }
  }
}