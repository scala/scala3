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
  trace("summarizing " + expr.show, init, s => Summary.show(s.asInstanceOf[Summary])) {
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
        val (pots, effs) = analyze(qualifier)
        if (env.ignoredMethods.contains(expr.symbol)) (Potentials.empty, effs)
        else if (!expr.symbol.exists) { // polymorphic function apply and structural types
          (Potentials.empty, pots.promote(expr) ++ effs)
        }
        else {
          val (pots2, effs2) = pots.select(expr.symbol, expr)
          (pots2, effs ++ effs2)
        }

      case _: This =>
        analyze(expr.tpe, expr)

      case Apply(fun, args) =>
        val summary = analyze(fun)
        val ignoredCall = env.ignoredMethods.contains(expr.symbol)

        val argTps = fun.tpe.widen match
          case mt: MethodType => mt.paramInfos

        val res = args.zip(argTps).foldLeft(summary) { case (sum, (arg, argTp)) =>
          val (pots1, effs1) = analyze(arg)
          if (ignoredCall) sum.withEffs(effs1)
          else if (argTp.isInstanceOf[ExprType]) sum + Promote(Fun(pots1, effs1)(arg))(arg)
          else sum.withEffs(pots1.promote(arg) ++ effs1)
        }

        if (ignoredCall) (Potentials.empty, res._2)
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
          val (pots, effs) = resolveThis(enclosing, thisRef, cur, expr)
          if pots.isEmpty then (Potentials.empty, effs)
          else {
            assert(pots.size == 1)
            (Warm(cls, pots.head)(expr).toPots, effs)
          }
        }
        else {
          val (pots, effs) = analyze(tref.prefix, expr)
          if (pots.isEmpty) Summary.empty.withEffs(effs)
          else {
            assert(pots.size == 1)
            (Warm(cls, pots.head)(expr).toPots, effs)
          }
        }

      case Typed(expr, tpt) =>
        if (tpt.tpe.hasAnnotation(defn.UncheckedAnnot)) Summary.empty
        else analyze(expr)

      case NamedArg(name, arg) =>
        analyze(arg)

      case Assign(lhs, rhs) =>
        val (pots, effs) = analyze(rhs)
        (Potentials.empty, pots.promote(expr) ++ effs)

      case closureDef(ddef) =>     // must be before `Block`
        val (pots, effs) = analyze(ddef.rhs)
        Summary.empty + Fun(pots, effs)(expr)

      case Block(stats, expr) =>
        val effs = stats.foldLeft(Effects.empty) { (acc, stat) => acc ++ analyze(stat)._2 }
        val (pots2, effs2) = analyze(expr)
        (pots2, effs ++ effs2)

      case If(cond, thenp, elsep) =>
        val (pots0, effs0) = analyze(cond)
        val (pots1, effs1) = analyze(thenp)
        val (pots2, effs2) = analyze(elsep)
        (pots0 ++ pots1 ++ pots2, effs0 ++ effs1 ++ effs2)

      case Annotated(arg, annot) =>
        if (expr.tpe.hasAnnotation(defn.UncheckedAnnot)) Summary.empty
        else analyze(arg)

      case Match(selector, cases) =>
        // possible for switches
        val (pots, effs) = analyze(selector)
        cases.foldLeft((Potentials.empty, pots.promote(selector) ++ effs)) { (acc, cas) =>
          acc union analyze(cas.body)
        }

      // case CaseDef(pat, guard, body) =>
      //   Summary.empty

      case Return(expr, from) =>
        val (pots, effs) = analyze(expr)
        (Potentials.empty, effs ++ pots.promote(expr))

      case WhileDo(cond, body) =>
        // for lazy fields, the translation may result in `while (<empty>)`
        val (_, effs1) = if (cond.isEmpty) Summary.empty else analyze(cond)
        val (_, effs2) = analyze(body)
        (Potentials.empty, effs1 ++ effs2)

      case Labeled(_, expr) =>
        val (_, effs1) = analyze(expr)
        (Potentials.empty, effs1)

      case Try(block, cases, finalizer) =>
        val (pots, effs) =  cases.foldLeft(analyze(block)) { (acc, cas) =>
          acc union analyze(cas.body)
        }
        val (_, eff2) = if (finalizer.isEmpty) Summary.empty else analyze(finalizer)
        (pots, effs ++ eff2)

      case SeqLiteral(elems, elemtpt) =>
        val effsAll: Effects = elems.foldLeft(Effects.empty) { (effs, elem) =>
          val (pots1, effs1) = analyze(elem)
          pots1.promote(expr) ++ effs1 ++ effs
        }
        (Potentials.empty, effsAll)

      case Inlined(call, bindings, expansion) =>
        val effs = bindings.foldLeft(Effects.empty) { (acc, mdef) => acc ++ analyze(mdef)._2 }
        analyze(expansion).withEffs(effs)

      case vdef : ValDef =>
        lazy val (pots, effs) = analyze(vdef.rhs)

        if (vdef.symbol.owner.isClass)
          (Potentials.empty, if (vdef.symbol.is(Flags.Lazy)) Effects.empty else effs)
        else
          (Potentials.empty, pots.promote(vdef) ++ effs)

      case Thicket(List()) =>
        // possible in try/catch/finally, see tests/crash/i6914.scala
        Summary.empty

      case ddef : DefDef =>
        lazy val (pots, effs) = analyze(ddef.rhs)

        if (ddef.symbol.owner.isClass) Summary.empty
        else (Potentials.empty, pots.promote(ddef) ++ effs)

      case _: TypeDef =>
        Summary.empty

      case _: Import =>
        Summary.empty

      case _ =>
        throw new Exception("unexpected tree: " + expr.show)
    }

    if (env.isAlwaysInitialized(expr.tpe)) (Potentials.empty, summary._2)
    else summary
  }

  def analyze(tp: Type, source: Tree)(implicit env: Env): Summary =
  trace("summarizing " + tp.show, init, s => Summary.show(s.asInstanceOf[Summary])) {
    val summary: Summary = tp match {
      case _: ConstantType =>
        Summary.empty

      case tmref: TermRef if tmref.prefix == NoPrefix =>
        Summary.empty

      case tmref: TermRef =>
        val (pots, effs) = analyze(tmref.prefix, source)
        if (env.ignoredMethods.contains(tmref.symbol)) (Potentials.empty, effs)
        else {
          val (pots2, effs2) = pots.select(tmref.symbol, source)
          (pots2, effs ++ effs2)
        }

      case ThisType(tref) =>
        val enclosing = env.ctx.owner.lexicallyEnclosingClass.asClass
        val cls = tref.symbol.asClass
        resolveThis(cls, ThisRef()(source), enclosing, source)

      case SuperType(thisTp, superTp) =>
        val (pots, effs) = analyze(thisTp, source)
        val pots2 = pots.map {
          // TODO: properly handle super of the form A & B
          SuperRef(_, superTp.classSymbols.head.asClass)(source): Potential
        }
        (pots2, effs)

      case _ =>
        throw new Exception("unexpected type: " + tp.show)
    }

    if (env.isAlwaysInitialized(tp)) (Potentials.empty, summary._2)
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
  trace("resolve " + cls.show + ", pot = " + pot.show + ", cur = " + cur.show, init, s => Summary.show(s.asInstanceOf[Summary])) {
    if (cls.is(Flags.Package)) Summary.empty
    else if (cls == cur) (pot.toPots, Effects.empty)
    else if (pot.size > 2) (Potentials.empty, Promote(pot)(source).toEffs)
    else {
      val enclosing = cur.owner.lexicallyEnclosingClass.asClass
      // Dotty uses O$.this outside of the object O
      if (enclosing.is(Flags.Package) && cls.is(Flags.Module)) return Summary.empty

      assert(!enclosing.is(Flags.Package), "enclosing = " + enclosing.show + ", cls = " + cls.show + ", pot = " + pot.show + ", cur = " + cur.show)
      val pot2 = Outer(pot, cur)(pot.source)
      resolveThis(cls, pot2, enclosing, source)
    }
  }

  /** Summarize secondary constructors or class body */
  def analyzeConstructor(ctor: Symbol)(implicit env: Env): Summary =
  trace("summarizing constructor " + ctor.owner.show, init, s => Summary.show(s.asInstanceOf[Summary])) {
    if (ctor.isPrimaryConstructor) {
      val cls = ctor.owner.asClass
      val tpl = ctor.owner.defTree.asInstanceOf[TypeDef].rhs.asInstanceOf[Template]
      val effs = analyze(Block(tpl.body, unitLiteral))._2

      def parentArgEffsWithInit(stats: List[Tree], ctor: Symbol, source: Tree): Effects =
        val initCall = MethodCall(ThisRef()(source), ctor)(source)
        stats.foldLeft(Set(initCall)) { (acc, stat) =>
          val (_, effs) = Summarization.analyze(stat)
          acc ++ effs
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
            if (cls == defn.AnyClass || cls == defn.AnyValClass) Effects.empty
            else {
              val ctor = cls.primaryConstructor
              Summarization.analyze(New(ref.tpe))(env.withOwner(ctor.owner))._2 +
                MethodCall(ThisRef()(ref), ctor)(ref)
            }
        })
      }

      (Potentials.empty, effsAll)
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
        parentCls -> analyze(tref.prefix, source)(env2)._1
      else
        parentCls -> analyze(cls.owner.lexicallyEnclosingClass.thisType, source)(env2)._1
    }

    if (cls.defTree.isEmpty)
      cls.info match {
        case cinfo: ClassInfo =>
          val source = {
            implicit val ctx2: Context = theCtx.withSource(cls.source(using theCtx))
            TypeTree(cls.typeRef).withSpan(cls.span)
          }

          val parentOuter =  cinfo.classParents.map { extractParentOuters(_, source) }.toMap
          ClassSummary(cls, parentOuter)
      }
    else {
      val tpl = cls.defTree.asInstanceOf[TypeDef]
      val parents = tpl.rhs.asInstanceOf[Template].parents

      val parentOuter = parents.map { parent => extractParentOuters(parent.tpe, parent) }
      ClassSummary(cls, parentOuter.toMap)
    }
  }

}
