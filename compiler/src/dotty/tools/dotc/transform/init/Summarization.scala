package dotty.tools.dotc
package transform
package init

import core._
import Contexts._
import Decorators._
import StdNames._
import Symbols._
import Constants.Constant
import Types._

import ast.tpd._
import config.Printers.init
import reporting.trace

import Effects._, Potentials._, Summary._, Util._

object Summarization {

  /** Summarization of potentials and effects for an expression
   *
   *  Optimization:
   *
   *   1. potentials for expression of primitive value types can be
   *      safely abandoned, as they are always fully initialized.
   */
  def analyze(expr: Tree)(implicit env: Env): Summary =
  trace("summarizing " + expr.show, init, s => s.asInstanceOf[Summary].show) {
    val summary: Summary = expr match {
      case Ident(nme.WILDCARD) =>
        // TODO:  disallow `var x: T = _`
        Summary.empty

      case Ident(name) =>
        assert(name.isTermName, "type trees should not reach here")
        analyze(expr.tpe, expr)

      case supert: Super =>
        analyze(supert.tpe, supert)

      case Select(qualifier, name) =>
        val Summary(pots, effs) = analyze(qualifier)
        if (env.canIgnoreMethod(expr.symbol)) Summary(effs)
        else if (!expr.symbol.exists) { // polymorphic function apply and structural types
          Summary(pots.promote(expr) ++ effs)
        }
        else {
          val Summary(pots2, effs2) = pots.select(expr.symbol, expr)
          Summary(pots2, effs ++ effs2)
        }

      case _: This =>
        analyze(expr.tpe, expr)

      case Apply(fun, args) =>
        val summary = analyze(fun)
        val ignoredCall = env.canIgnoreMethod(expr.symbol)

        val argTps = fun.tpe.widen match
          case mt: MethodType => mt.paramInfos

        val res = args.zip(argTps).foldLeft(summary) { case (sum, (arg, argTp)) =>
          val Summary(pots1, effs1) = analyze(arg)
          if (ignoredCall) sum ++ effs1
          else if (argTp.isInstanceOf[ExprType]) sum + Promote(Fun(pots1, effs1)(arg))(arg)
          else sum ++ pots1.promote(arg) ++ effs1
        }

        if (ignoredCall) Summary(res.effs)
        else res

      case TypeApply(fun, args) =>
        analyze(fun)

      case Literal(const) =>
        Summary.empty

      case New(tpt) =>
        def typeRefOf(tp: Type): TypeRef = tp.dealias.typeConstructor match {
          case tref: TypeRef => tref
          case hklambda: HKTypeLambda => typeRefOf(hklambda.resType)
        }

        val tref = typeRefOf(tpt.tpe)
        val cls = tref.classSymbol.asClass
        // local class may capture, thus we need to track it
        if (tref.prefix == NoPrefix) {
          val cur = theCtx.owner.lexicallyEnclosingClass.asClass
          val thisRef = ThisRef()(expr)
          val enclosing = cls.owner.lexicallyEnclosingClass.asClass
          val summary = resolveThis(enclosing, thisRef, cur, expr)
          if summary.pots.isEmpty then summary
          else {
            assert(summary.pots.size == 1)
            summary.dropPotentials + Warm(cls, summary.pots.head)(expr)
          }
        }
        else {
          val summary = analyze(tref.prefix, expr)
          if summary.pots.isEmpty then summary
          else {
            assert(summary.pots.size == 1)
            summary.dropPotentials + Warm(cls, summary.pots.head)(expr)
          }
        }

      case Typed(expr, tpt) =>
        if (tpt.tpe.hasAnnotation(defn.UncheckedAnnot)) Summary.empty
        else analyze(expr) ++ effectsOfType(tpt.tpe, tpt)

      case NamedArg(name, arg) =>
        analyze(arg)

      case Assign(lhs, rhs) =>
        val Summary(pots, effs) = analyze(rhs)
        Summary(pots.promote(expr) ++ effs)

      case closureDef(ddef) =>     // must be before `Block`
        val Summary(pots, effs) = analyze(ddef.rhs)
        Summary(Fun(pots, effs)(expr))

      case Block(stats, expr) =>
        val effs = stats.foldLeft(Effects.empty) { (acc, stat) => acc ++ analyze(stat).effs }
        val Summary(pots2, effs2) = analyze(expr)
        Summary(pots2, effs ++ effs2)

      case If(cond, thenp, elsep) =>
        val Summary(_, effs0) = analyze(cond)
        val Summary(pots1, effs1) = analyze(thenp)
        val Summary(pots2, effs2) = analyze(elsep)
        Summary(pots1 ++ pots2, effs0 ++ effs1 ++ effs2)

      case Annotated(arg, annot) =>
        if (expr.tpe.hasAnnotation(defn.UncheckedAnnot)) Summary.empty
        else analyze(arg)

      case Match(selector, cases) =>
        // possible for switches
        val Summary(pots, effs) = analyze(selector)
        val init = Summary(Potentials.empty, pots.promote(selector) ++ effs)
        cases.foldLeft(init) { (acc, cas) =>
          acc + analyze(cas.body)
        }

      // case CaseDef(pat, guard, body) =>
      //   Summary.empty

      case Return(expr, from) =>
        val Summary(pots, effs) = analyze(expr)
        Summary(effs ++ pots.promote(expr))

      case WhileDo(cond, body) =>
        // for lazy fields, the translation may result in `while (<empty>)`
        val Summary(_, effs1) = if (cond.isEmpty) Summary.empty else analyze(cond)
        val Summary(_, effs2) = analyze(body)
        Summary(effs1 ++ effs2)

      case Labeled(_, expr) =>
        val summary = analyze(expr)
        summary.dropPotentials

      case Try(block, cases, finalizer) =>
        val Summary(pots, effs) =  cases.foldLeft(analyze(block)) { (acc, cas) =>
          acc + analyze(cas.body)
        }
        val Summary(_, eff2) = if (finalizer.isEmpty) Summary.empty else analyze(finalizer)
        Summary(pots, effs ++ eff2)

      case SeqLiteral(elems, elemtpt) =>
        val effsAll: Effects = elems.foldLeft(Effects.empty) { (effs, elem) =>
          val Summary(pots1, effs1) = analyze(elem)
          pots1.promote(expr) ++ effs1 ++ effs
        }
        Summary(effsAll)

      case Inlined(call, bindings, expansion) =>
        val effs = bindings.foldLeft(Effects.empty) { (acc, mdef) =>
          acc ++ analyze(mdef).effs
        }
        analyze(expansion) ++ effs

      case vdef : ValDef =>
        val Summary(pots, effs) = analyze(vdef.rhs)

        if (vdef.symbol.owner.isClass)
          if (vdef.symbol.is(Flags.Lazy)) Summary.empty else Summary(effs)
        else
          Summary(pots.promote(vdef) ++ effs)

      case Thicket(List()) =>
        // possible in try/catch/finally, see tests/crash/i6914.scala
        Summary.empty

      case ddef : DefDef =>
        if (ddef.symbol.owner.isClass) Summary.empty
        else {
          val Summary(pots, effs) = analyze(ddef.rhs)
          Summary(pots.promote(ddef) ++ effs)
        }

      case tdef: TypeDef =>
        if tdef.isClassDef then Summary.empty
        else Summary(effectsOfType(tdef.symbol.info, tdef.rhs))

      case _: Import | _: Export =>
        Summary.empty

      case _ =>
        throw new Exception("unexpected tree: " + expr.show)
    }

    if (env.isAlwaysInitialized(expr.tpe)) Summary(Potentials.empty, summary.effs)
    else summary
  }

  private def effectsOfType(tp: Type, source: Tree)(implicit env: Env): Effects =
    var summary = Summary.empty
    val traverser = new TypeTraverser {
      def traverse(tp: Type): Unit = tp match {
        case TermRef(_: SingletonType, _) =>
          summary = summary + analyze(tp, source)
        case _ =>
          traverseChildren(tp)
      }
    }
    traverser.traverse(tp)
    summary.effs

  def analyze(tp: Type, source: Tree)(implicit env: Env): Summary =
  trace("summarizing " + tp.show, init, s => s.asInstanceOf[Summary].show) {
    val summary: Summary = tp match {
      case _: ConstantType =>
        Summary.empty

      case tmref: TermRef if tmref.prefix == NoPrefix =>
        Summary.empty

      case tmref: TermRef =>
        val Summary(pots, effs) = analyze(tmref.prefix, source)
        if (env.canIgnoreMethod(tmref.symbol)) Summary(effs)
        else {
          val summary = pots.select(tmref.symbol, source)
          summary ++ effs
        }

      case ThisType(tref) =>
        val enclosing = env.ctx.owner.lexicallyEnclosingClass.asClass
        val cls = tref.symbol.asClass
        resolveThis(cls, ThisRef()(source), enclosing, source)

      case SuperType(thisTp, superTp) =>
        val Summary(pots, effs) = analyze(thisTp, source)
        val pots2 = pots.map {
          // TODO: properly handle super of the form A & B
          SuperRef(_, superTp.classSymbols.head.asClass)(source): Potential
        }
        Summary(pots2, effs)

      case _: TermParamRef | _: RecThis  =>
        // possible from checking effects of types
        Summary.empty

      case _ =>
        throw new Exception("unexpected type: " + tp)
    }

    if (env.isAlwaysInitialized(tp)) Summary(Potentials.empty, summary.effs)
    else summary
  }

  def analyzeMethod(sym: Symbol)(implicit env: Env): Summary = {
    val ddef = sym.defTree.asInstanceOf[DefDef]
    traceIndented(sym.show + " = " + ddef.show, init)
    analyze(ddef.rhs)(env.withOwner(sym))
  }

  def analyzeField(sym: Symbol)(implicit env: Env): Summary = {
    val vdef = sym.defTree.asInstanceOf[ValDef]
    analyze(vdef.rhs)(env.withOwner(sym))
  }

  def resolveThis(cls: ClassSymbol, pot: Potential, cur: ClassSymbol, source: Tree)(implicit env: Env): Summary =
  trace("resolve " + cls.show + ", pot = " + pot.show + ", cur = " + cur.show, init, s => s.asInstanceOf[Summary].show) {
    if (cls.is(Flags.Package)) Summary.empty
    else if (cls == cur) Summary(pot)
    else if (pot.size > 2) Summary(Promote(pot)(source))
    else {
      val enclosing = cur.owner.lexicallyEnclosingClass.asClass
      // Dotty uses O$.this outside of the object O
      if (enclosing.is(Flags.Package) && cls.is(Flags.Module))
        return Summary.empty

      assert(!enclosing.is(Flags.Package), "enclosing = " + enclosing.show + ", cls = " + cls.show + ", pot = " + pot.show + ", cur = " + cur.show)
      val pot2 = Outer(pot, cur)(pot.source)
      resolveThis(cls, pot2, enclosing, source)
    }
  }

  /** Summarize secondary constructors or class body */
  def analyzeConstructor(ctor: Symbol)(implicit env: Env): Summary =
  trace("summarizing constructor " + ctor.owner.show, init, s => s.asInstanceOf[Summary].show) {
    if (ctor.isPrimaryConstructor) {
      val cls = ctor.owner.asClass
      val tpl = ctor.owner.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]
      val effs = analyze(Block(tpl.body, unitLiteral)).effs

      def parentArgEffsWithInit(stats: List[Tree], ctor: Symbol, source: Tree): Effects =
        val init =
          if env.canIgnoreMethod(ctor) then Effects.empty
          else Effects.empty :+ MethodCall(ThisRef()(source), ctor)(source)
        stats.foldLeft(init) { (acc, stat) =>
          val summary = Summarization.analyze(stat)
          acc ++ summary.effs
        }

      val effsAll = tpl.parents.foldLeft(effs) { (effs, parent) =>
        effs ++ (parent match {
          case tree @ Block(stats, parent) =>
            val ctor @ Select(qual, _) = funPart(parent)
            parentArgEffsWithInit(qual :: stats ++ termArgss(parent).flatten, ctor.symbol, tree)

          case tree @ Apply(Block(stats, parent), args) =>
            val ctor @ Select(qual, _) = funPart(parent)
            parentArgEffsWithInit(qual :: stats ++ args ++ termArgss(parent).flatten, ctor.symbol, tree)

          case parent : Apply =>
            val ctor @ Select(qual, _) = funPart(parent)
            parentArgEffsWithInit(qual :: termArgss(parent).flatten, ctor.symbol, parent)

          case ref =>
            val tref: TypeRef = ref.tpe.typeConstructor.asInstanceOf
            val cls = tref.classSymbol.asClass
            if env.canIgnoreClass(cls) then Effects.empty
            else {
              val ctor = cls.primaryConstructor
              val prefixEff =
                if tref.prefix == NoPrefix then Effects.empty
                else Summarization.analyze(tref.prefix, ref).effs

              prefixEff :+ MethodCall(ThisRef()(ref), ctor)(ref)
            }
        })
      }

      Summary(effsAll)
    }
    else {
      val ddef = ctor.defTree.asInstanceOf[DefDef]
      analyze(ddef.rhs)(env.withOwner(ctor))
    }
  }

  def classSummary(cls: ClassSymbol)(implicit env: Env): ClassSummary = trace("summarizing " + cls.show, init) {
    def extractParentOuters(parent: Type, source: Tree): (ClassSymbol, Potentials) = {
      val tref = parent.typeConstructor.stripAnnots.asInstanceOf[TypeRef]
      val parentCls = tref.classSymbol.asClass
      val env2: Env = env.withOwner(cls.owner.lexicallyEnclosingClass)
      if (tref.prefix != NoPrefix)
        parentCls -> analyze(tref.prefix, source)(env2).pots
      else
        parentCls -> analyze(cls.owner.lexicallyEnclosingClass.thisType, source)(env2).pots
    }

    if (cls.defTree.isEmpty)
        val source = {
          implicit val ctx2: Context = theCtx.withSource(cls.source(using theCtx))
          TypeTree(cls.typeRef).withSpan(cls.span)
        }

        val parentOuter = cls.info.parents.map { extractParentOuters(_, source) }.toMap
        ClassSummary(cls, parentOuter)
    else {
      val tpl = cls.defTree.asInstanceOf[TypeDef]
      val parents = tpl.rhs.asInstanceOf[Template].parents

      val parentOuter = parents.map { parent => extractParentOuters(parent.tpe, parent) }
      ClassSummary(cls, parentOuter.toMap)
    }
  }

}
