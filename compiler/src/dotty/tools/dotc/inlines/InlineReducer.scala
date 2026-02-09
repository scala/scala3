package dotty.tools
package dotc
package inlines

import ast.*, core.*
import Flags.*, Symbols.*, Types.*, Decorators.*, Contexts.*
import StdNames.nme
import typer.*
import Names.TermName
import NameKinds.{InlineAccessorName, InlineBinderName, InlineScrutineeName}
import config.Printers.inlining
import util.SimpleIdentityMap
import CheckRealizable.{Realizable, realizability}

import collection.mutable

/** A utility class offering methods for rewriting inlined code */
class InlineReducer(inliner: Inliner)(using Context):
  import tpd.*
  import Inliner.{isElideableExpr, DefBuffer}
  import inliner.{call, newSym, tryInlineArg, paramBindingDef}

  extension (tp: Type)
    /** same as widenTermRefExpr, but preserves modules and singleton enum values */
    private def widenInlineScrutinee(using Context): Type = tp.stripTypeVar match
      case tp: TermRef  =>
        val sym = tp.termSymbol
        if sym.isAllOf(EnumCase, butNot=JavaDefined) || sym.is(Module) then tp
        else if !tp.isOverloaded then tp.underlying.widenExpr.widenInlineScrutinee
        else tp
      case _ => tp

  /** An extractor for terms equivalent to `new C(args)`, returning the class `C`,
   *  a list of bindings, and the arguments `args`. Can see inside blocks and Inlined nodes and can
   *  follow a reference to an inline value binding to its right hand side.
   *
   *  @return    optionally, a triple consisting of
   *             - the class `C`
   *             - the arguments `args`
   *             - any bindings that wrap the instance creation
   *             - whether the instance creation is precomputed or by-name
   */
  private object NewInstance {
    def unapply(tree: Tree)(using Context): Option[(Symbol, List[Tree], List[Tree], Boolean)] = {
      def unapplyLet(bindings: List[Tree], expr: Tree) =
        unapply(expr) map {
          case (cls, reduced, prefix, precomputed) => (cls, reduced, bindings ::: prefix, precomputed)
        }
      tree match {
        case Apply(fn, args) =>
          fn match {
            case Select(New(tpt), nme.CONSTRUCTOR) =>
              Some((tpt.tpe.classSymbol, args, Nil, false))
            case TypeApply(Select(New(tpt), nme.CONSTRUCTOR), _) =>
              Some((tpt.tpe.classSymbol, args, Nil, false))
            case _ =>
              val meth = fn.symbol
              if (meth.name == nme.apply &&
                  meth.flags.is(Synthetic) &&
                  meth.owner.linkedClass.is(Case))
                Some(meth.owner.linkedClass, args, Nil, false)
              else None
          }
        case Typed(inner, _) =>
          // drop the ascribed tpt. We only need it if we can't find a NewInstance
          unapply(inner)
        case Ident(_) =>
          val binding = tree.symbol.defTree
          for ((cls, reduced, prefix, precomputed) <- unapply(binding))
          yield (cls, reduced, prefix, precomputed || binding.isInstanceOf[ValDef])
        case Inlined(_, bindings, expansion) =>
          unapplyLet(bindings, expansion)
        case Block(stats, expr) if isElideableExpr(tree) =>
          unapplyLet(stats, expr)
        case _ =>
          None
      }
    }
  }

  /** If `tree` is equivalent to `new C(args).x` where class `C` does not have
   *  initialization code and `x` is a parameter corresponding to one of the
   *  arguments `args`, the corresponding argument, otherwise `tree` itself.
   *  Side effects of original arguments need to be preserved.
   */
  def reduceProjection(tree: Tree)(using Context): Tree = {
    if (ctx.debug) inlining.println(i"try reduce projection $tree")
    tree match {
      case Select(NewInstance(cls, args, prefix, precomputed), field) if cls.isNoInitsRealClass =>
        def matches(param: Symbol, selection: Symbol): Boolean =
          param == selection || {
            selection.name match {
              case InlineAccessorName(underlying) =>
                param.name == underlying && selection.info.isInstanceOf[ExprType]
              case _ =>
                false
            }
          }
        val idx = cls.asClass.paramAccessors.indexWhere(matches(_, tree.symbol))
        if (idx >= 0 && idx < args.length) {
          def finish(arg: Tree) =
            new TreeTypeMap().transform(arg) // make sure local bindings in argument have fresh symbols
              .showing(i"projecting $tree -> $result", inlining)
          val arg = args(idx)
          if (precomputed)
            if (isElideableExpr(arg)) finish(arg)
            else tree // nothing we can do here, projection would duplicate side effect
          else {
            // newInstance is evaluated in place, need to reflect side effects of
            // arguments in the order they were written originally
            def collectImpure(from: Int, end: Int) =
              (from until end).filterNot(i => isElideableExpr(args(i))).toList.map(args)
            val leading = collectImpure(0, idx)
            val trailing = collectImpure(idx + 1, args.length)
            val argInPlace =
              if (trailing.isEmpty) arg
              else
                def argsSpan = trailing.map(_.span).foldLeft(arg.span)(_.union(_))
                letBindUnless(TreeInfo.Pure, arg)(Block(trailing, _).withSpan(argsSpan))
            val blockSpan = (prefix ::: leading).map(_.span).foldLeft(argInPlace.span)(_.union(_))
            finish(seq(prefix, seq(leading, argInPlace)).withSpan(blockSpan))
          }
        }
        else tree
      case Block(stats, expr) if stats.forall(isPureBinding) =>
        cpy.Block(tree)(stats, reduceProjection(expr))
      case _ => tree
    }
  }

  /** If this is a value binding:
    *   - reduce its rhs if it is a projection and adjust its type accordingly,
    *   - record symbol -> rhs in the InlineBindings context propery.
    */
  def normalizeBinding(binding: ValOrDefDef)(using Context) = {
    val binding1 = binding match {
      case binding: ValDef =>
        val rhs1 = reduceProjection(binding.rhs)
        binding.symbol.defTree = rhs1
        if (rhs1 `eq` binding.rhs) binding
        else {
          binding.symbol.info = rhs1.tpe
          cpy.ValDef(binding)(tpt = TypeTree(rhs1.tpe), rhs = rhs1)
        }
      case _ =>
        binding
    }
    binding1.withSpan(call.span)
  }

  /** The result type of reducing a match. It consists optionally of a list of bindings
   *  for the pattern-bound variables and the RHS of the selected case.
   *  Returns `None` if no case was selected.
   */
  type MatchRedux = Option[(List[MemberDef], Tree)]

  /** Same as MatchRedux, but also includes a boolean
   *  that is true if the guard can be checked at compile time.
   */
  type MatchReduxWithGuard = Option[(List[MemberDef], Tree, Boolean)]

  /** Reduce an inline match
    *   @param     mtch          the match tree
    *   @param     scrutinee     the scrutinee expression, assumed to be pure, or
    *                            EmptyTree for a summonFrom
    *   @param     scrutType     its fully defined type, or
    *                            ImplicitScrutineeTypeRef for a summonFrom
    *   @param     typer         The current inline typer
    *   @return    optionally, if match can be reduced to a matching case: A pair of
    *              bindings for all pattern-bound variables and the RHS of the case.
    */
  def reduceInlineMatch(scrutinee: Tree, scrutType: Type, cases: List[CaseDef], typer: Typer)(using Context): MatchRedux = {

    val isImplicit = scrutinee.isEmpty

    val unusable: util.EqHashSet[Symbol] = util.EqHashSet()

    /** Adjust internaly generated value definitions;
     *   - If the RHS refers to an erased symbol, mark the val as erased
     *   - If the RHS refers to an unusable symbol, mark the val as unusable
     */
    def adjustErased(sym: TermSymbol, rhs: Tree): Unit =
      rhs.foreachSubTree:
        case id: Ident if id.symbol.isErased =>
          sym.setFlag(Erased)
          if unusable.contains(id.symbol) then unusable += sym
        case _ =>

    /** Try to match pattern `pat` against scrutinee reference `scrut`. If successful add
     *  bindings for variables bound in this pattern to `caseBindingMap`.
     */
    def reducePattern(
      caseBindingMap: mutable.ListBuffer[(Symbol, MemberDef)],
      scrut: TermRef,
      pat: Tree
    )(using Context): Boolean = {

      /** Create a binding of a pattern bound variable with matching part of
       *  scrutinee as RHS and type that corresponds to RHS.
       */
      def newTermBinding(sym: TermSymbol, rhs: Tree): Unit =
        val copied = sym.copy(info = rhs.tpe.widenInlineScrutinee, coord = sym.coord,
          flags = sym.flags &~ Case).asTerm
        adjustErased(copied, rhs)
        caseBindingMap += ((sym, ValDef(copied, constToLiteral(rhs)).withSpan(sym.span)))

      def newTypeBinding(sym: TypeSymbol, alias: Type): Unit = {
        val copied = sym.copy(info = TypeAlias(alias), coord = sym.coord).asType
        caseBindingMap += ((sym, TypeDef(copied)))
      }

      def searchImplicit(sym: TermSymbol, tpt: Tree) = {
        val evTyper = new Typer(ctx.nestingLevel + 1)
        val evCtx = ctx.fresh.setTyper(evTyper)
        inContext(evCtx) {
          val evidence = evTyper.inferImplicitArg(tpt.tpe, tpt.span)
          evidence.tpe match {
            case fail: Implicits.AmbiguousImplicits =>
              report.error(evTyper.missingArgMsg(evidence, tpt.tpe, ""), tpt.srcPos)
              true // hard error: return true to stop implicit search here
            case fail: Implicits.SearchFailureType =>
              false
            case _ =>
              //inlining.println(i"inferred implicit $sym: ${sym.info} with $evidence: ${evidence.tpe.widen}, ${evCtx.gadt.constraint}, ${evCtx.typerState.constraint}")
              newTermBinding(sym, evidence)
              true
          }
        }
      }

      type TypeBindsMap = SimpleIdentityMap[TypeSymbol, java.lang.Boolean]

      def getTypeBindsMap(pat: Tree, tpt: Tree): TypeBindsMap = {
        // Extractors can contain Bind nodes in type parameter lists,
        // for that case tree looks like this:
        //   UnApply[t @ t](pats)(implicits): T[t]
        // Test case is pos/inline-caseclass.scala.
        //
        // Alternatively, for explicitly specified type binds in type annotations like in
        //   case A(B): A[t]
        // the tree will look like this:
        //   Unapply[t](pats)(implicits) : T[t @ t]
        // and the binds will be found in the type tree instead
        // Test case is pos-macros/i15971
        val tptBinds = tpt.bindTypeSymbols.toSet
        val binds: Set[TypeSymbol] = pat match {
          case UnApply(TypeApply(_, tpts), _, _) =>
            tpts.flatMap(_.bindTypeSymbols).toSet ++ tptBinds
          case _ => tptBinds
        }

        val extractBindVariance = new TypeAccumulator[TypeBindsMap] {
          def apply(syms: TypeBindsMap, t: Type) = {
            val syms1 = t match {
              // `binds` is used to check if the symbol was actually bound by the pattern we're processing
              case tr: TypeRef if tr.symbol.is(Case) && binds.contains(tr.symbol.asType) =>
                val trSym = tr.symbol.asType
                // Exact same logic as in IsFullyDefinedAccumulator:
                // the binding is to be maximized iff it only occurs contravariantly in the type
                val wasToBeMinimized: Boolean = {
                  val v = syms(trSym)
                  if (v != null) v else false
                }
                syms.updated(trSym, wasToBeMinimized || variance >= 0 : java.lang.Boolean)
              case _ =>
                syms
            }
            foldOver(syms1, t)
          }
        }

        extractBindVariance(SimpleIdentityMap.empty, tpt.tpe)
      }

      def addTypeBindings(typeBinds: TypeBindsMap)(using Context): Unit =
        typeBinds.foreachBinding { case (sym, shouldBeMinimized) =>
          newTypeBinding(sym,
            ctx.gadtState.approximation(sym, fromBelow = shouldBeMinimized, maxLevel = Int.MaxValue))
        }

      def registerAsGadtSyms(typeBinds: TypeBindsMap)(using Context): Unit =
        if (typeBinds.size > 0) ctx.gadtState.addToConstraint(typeBinds.keys)

      pat match {
        case Typed(pat1, tpt) =>
          val typeBinds = getTypeBindsMap(pat1, tpt)
          registerAsGadtSyms(typeBinds)
          scrut <:< tpt.tpe && {
            addTypeBindings(typeBinds)
            reducePattern(caseBindingMap, scrut, pat1)
          }
        case pat @ Bind(name: TermName, Typed(_, tpt)) if isImplicit =>
          val typeBinds = getTypeBindsMap(tpt, tpt)
          registerAsGadtSyms(typeBinds)
          searchImplicit(pat.symbol.asTerm, tpt) && {
            addTypeBindings(typeBinds)
            true
          }
        case pat @ Bind(name: TermName, body) =>
          reducePattern(caseBindingMap, scrut, body) && {
            if (name != nme.WILDCARD) newTermBinding(pat.symbol.asTerm, ref(scrut))
            true
          }
        case Ident(nme.WILDCARD) =>
          true
        case pat: Literal =>
          scrut.widenTermRefExpr =:= pat.tpe
        case pat: RefTree =>
          scrut =:= pat.tpe ||
          scrut.classSymbol.is(Module) && scrut.widen =:= pat.tpe.widen && {
            scrut.prefix match {
              case _: SingletonType | NoPrefix => true
              case _ => false
            }
          }
        case UnApply(unapp, _, pats) =>
          unapp.tpe.widen match {
            case mt: MethodType if mt.paramInfos.length == 1 =>

              def reduceSubPatterns(pats: List[Tree], selectors: List[Tree]): Boolean = (pats, selectors) match {
                case (Nil, Nil) => true
                case (pat :: pats1, selector :: selectors1) =>
                  val elem = newSym(InlineBinderName.fresh(), Synthetic, selector.tpe.widenInlineScrutinee).asTerm
                  adjustErased(elem, selector)
                  val rhs = constToLiteral(selector)
                  elem.defTree = rhs
                  caseBindingMap += ((NoSymbol, ValDef(elem, rhs).withSpan(elem.span)))
                  reducePattern(caseBindingMap, elem.termRef, pat) &&
                  reduceSubPatterns(pats1, selectors1)
                case _ => false
              }

              val paramType = mt.paramInfos.head
              val paramCls = paramType.classSymbol
              if (paramCls.is(Case) && unapp.symbol.is(Synthetic) && scrut <:< paramType) {
                val caseAccessors =
                  if paramCls.is(Scala2x) then paramCls.caseAccessors
                  else paramCls.asClass.paramAccessors
                val selectors =
                  for (accessor <- caseAccessors)
                  yield constToLiteral(reduceProjection(ref(scrut).select(accessor).ensureApplied))
                caseAccessors.length == pats.length && reduceSubPatterns(pats, selectors)
              }
              else false
            case _ =>
              false
          }
        case Alternative(pats) =>
          pats.exists(reducePattern(caseBindingMap, scrut, _))
        case tree: Inlined if tree.inlinedFromOuterScope =>
          reducePattern(caseBindingMap, scrut, tree.expansion)
        case _ => false
      }
    }

    /** The initial scrutinee binding: `val $scrutineeN = <scrutinee>` */
    val scrutineeSym = newSym(InlineScrutineeName.fresh(), Synthetic, scrutType).asTerm
    val scrutineeBinding = normalizeBinding(ValDef(scrutineeSym, scrutinee))

    // If scrutinee has embedded references to `compiletime.erasedValue` or to
    // other erased values, mark scrutineeSym as Erased. In addition, if scrutinee
    // is not a pure expression, mark scrutineeSym as unusable. The reason is that
    // scrutinee would then fail the tests in erasure that demand that the RHS of
    // an erased val is a pure expression. At the end of the inline match reduction
    // we throw out all unusable vals and check that the remaining code does not refer
    // to unusable symbols.
    // Note that compiletime.erasedValue is treated as erased but not pure, so scrutinees
    // containing references to it becomes unusable.
    if scrutinee.existsSubTree(_.symbol.isErased) then
      scrutineeSym.setFlag(Erased)
      if !tpd.isPureExpr(scrutinee) then unusable += scrutineeSym

    def reduceCase(cdef: CaseDef): MatchReduxWithGuard = {
      val caseBindingMap = new mutable.ListBuffer[(Symbol, MemberDef)]()

      def substBindings(bindings: List[(Symbol, MemberDef)]): (List[MemberDef], List[Symbol], List[Symbol]) =
        val (from, to) = bindings.collect { case (sym, bnd) if sym.exists => (sym, bnd.symbol) }.unzip
        to.foreach(sym => sym.info = sym.info.substSym(from, to))
        val substituted = bindings.map { case (sym, bnd) => bnd.subst(from, to) }
        (substituted, from, to)

      if (!isImplicit) caseBindingMap += ((NoSymbol, scrutineeBinding))
      val gadtCtx = ctx.fresh.setFreshGADTBounds.addMode(Mode.GadtConstraintInference)
      if (reducePattern(caseBindingMap, scrutineeSym.termRef, cdef.pat)(using gadtCtx)) {
        val (caseBindings, from, to) = substBindings(caseBindingMap.toList)
        val (guardOK, canReduceGuard) =
          if cdef.guard.isEmpty then (true, true)
          else stripInlined(typer.typed(cdef.guard.subst(from, to), defn.BooleanType)) match {
            case ConstantValue(v: Boolean) => (v, true)
            case _ => (false, false)
          }
        if !canReduceGuard then Some((List.empty, EmptyTree, false))
        else if !guardOK then None
        else cdef.body.subst(from, to) match
          case t: SubMatch => // a sub match of an inline match is also inlined
            reduceInlineMatch(t.selector, t.selector.tpe, t.cases, typer).map:
              (subCaseBindings, rhs) => (caseBindings ++ subCaseBindings, rhs, true)
          case b => Some((caseBindings, b, true))
      }
      else None
    }

    def recur(cases: List[CaseDef]): MatchRedux = cases match {
      case Nil => None
      case cdef :: cases1 =>
        reduceCase(cdef) match
          case None => recur(cases1)
          case r @ Some((caseBindings, rhs, canReduceGuard)) if canReduceGuard => Some((caseBindings, rhs))
          case _ => None
    }

    for (bindings, expr) <- recur(cases) yield
      // drop unusable vals and check that no referenes to unusable symbols remain
      val cleanupUnusable = new TreeMap:

        /** Whether we are currently in a type position */
        var inType: Boolean = false

        override def transform(tree: Tree)(using Context): Tree =
          tree match
            case tree: ValDef if unusable.contains(tree.symbol) => EmptyTree
            case id: Ident if unusable.contains(id.symbol) =>
              // This conditions allows references to erased values in type
              // positions provided the types of these references are
              // realizable. See erased-inline-product.scala and
              // tests/neg/erased-inline-unrealizable-path.scala.
              if !inType || (realizability(id.tpe.widen) ne Realizable) then
                report.error(
                  em"""${id.symbol} is unusable in ${ctx.owner} because it refers to an erased expression
                      |in the selector of an inline match that reduces to
                      |
                      |${Block(bindings, expr)}""",
                  tree.srcPos)
              tree
            case _ if tree.isType =>
              val saved = inType
              inType = true
              val tree1 = super.transform(tree)
              inType = saved
              tree1
            case _ =>
              super.transform(tree)

      val bindings1 = bindings.mapConserve(cleanupUnusable.transform).collect:
        case mdef: MemberDef => mdef
      (bindings1, cleanupUnusable.transform(expr))
  }
end InlineReducer

