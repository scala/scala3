package dotty.tools.dotc.transform.linker

import dotty.tools.dotc.{ast, core}
import core._
import Contexts._
import dotty.tools.dotc.ast.Trees._
import StdNames._
import NameOps._
import dotty.tools.dotc.ast.tpd
import Symbols._
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.core.Types.{ConstantType,ExprType, MethodType, MethodicType, NoPrefix, NoType, TermRef, ThisType, Type}
import dotty.tools.dotc.transform.{Erasure, TreeTransforms}
import dotty.tools.dotc.transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo, TreeTransform}
import dotty.tools.dotc.transform.SymUtils._
import Decorators._
import dotty.tools.dotc.core.Constants.Constant
import dotty.tools.dotc.core.DenotTransformers.IdentityDenotTransformer
import dotty.tools.dotc.typer.ConstFold

import scala.collection.mutable
import scala.collection.mutable.ListBuffer


class Simplify extends MiniPhaseTransform with IdentityDenotTransformer {
  import tpd._

  override def phaseName: String = "simplify"

  private var SeqFactoryClass: Symbol = null
  private var symmetricOperations: Set[Symbol] = null
  var optimize = false



  override def prepareForUnit(tree: _root_.dotty.tools.dotc.ast.tpd.Tree)(implicit ctx: Context): TreeTransform = {
    SeqFactoryClass = ctx.requiredClass("scala.collection.generic.SeqFactory")
    symmetricOperations = Set(defn.Boolean_&&, defn.Boolean_||, defn.Int_+, defn.Int_*, defn.Long_+, defn.Long_*)
    optimize = ctx.settings.optimise.value
    this
  }

  private def desugarIdent(i: Ident)(implicit ctx: Context): Option[tpd.Select] = {
    i.tpe match {
      case TermRef(prefix: TermRef, name) =>
        Some(tpd.ref(prefix).select(i.symbol))
      case TermRef(prefix: ThisType, name) =>
        Some(tpd.This(prefix.cls).select(i.symbol))
      case _ => None
    }
  }

  private def dropCasts(t: Tree)(implicit ctx: Context): Tree = t match {
    //case TypeApply(aio@Select(rec, nm), _) if aio.symbol == defn.Any_asInstanceOf => dropCasts(rec)
    case Typed(t, tpe) => t
    case _ => t
  }

  private def readingOnlyVals(t: Tree)(implicit ctx: Context): Boolean = dropCasts(t) match {
    case Typed(exp, tpe) => readingOnlyVals(exp)
    case TypeApply(fun @Select(rec, _), List(tp))
      if (fun.symbol eq defn.Any_asInstanceOf) && rec.tpe.derivesFrom(TypeErasure.erasure(tp.tpe).classSymbol) =>
      readingOnlyVals(rec)
    case Apply(Select(rec, _), Nil) =>
      if (t.symbol.isGetter && !t.symbol.is(Flags.Mutable)) readingOnlyVals(rec) // getter of a immutable field
      else if (t.symbol.owner.derivesFrom(defn.ProductClass) && t.symbol.owner.is(Flags.CaseClass) && t.symbol.name.isProductAccessorName)
        readingOnlyVals(rec) // accessing a field of a product
      else if (t.symbol.is(Flags.CaseAccessor) && !t.symbol.is(Flags.Mutable))
        readingOnlyVals(rec)
      else false
    case Select(rec, _) if t.symbol.is(Flags.Method) =>
      if (t.symbol.isGetter && !t.symbol.is(Flags.Mutable)) readingOnlyVals(rec) // getter of a immutable field
      else if (t.symbol.owner.derivesFrom(defn.ProductClass) && t.symbol.owner.is(Flags.CaseClass) && t.symbol.name.isProductAccessorName) {
        def isImmutableField = {
          val fieldId = t.symbol.name.drop(1).toString.toInt - 1
          !t.symbol.owner.caseAccessors(ctx)(fieldId).is(Flags.Mutable)
        }
        if (isImmutableField) readingOnlyVals(rec) // accessing a field of a product
        else false
      } else if (t.symbol.is(Flags.CaseAccessor) && !t.symbol.is(Flags.Mutable))
        readingOnlyVals(rec)
      else false
    case Select(qual, _) if !t.symbol.is(Flags.Mutable) =>
      readingOnlyVals(qual)
    case t: Ident if !t.symbol.is(Flags.Mutable) && !t.symbol.is(Flags.Method) =>
      desugarIdent(t) match {
        case Some(t) => readingOnlyVals(t)
        case None => true
      }
    case t: This => true
    case t: Literal => true
    case _ => false
  }

  type Visitor = Tree => Unit
  type ErasureCompatibility = Int
  val BeforeErasure: ErasureCompatibility = 1
  val AfterErasure: ErasureCompatibility  = 2
  val BeforeAndAfterErasure: ErasureCompatibility  = BeforeErasure | AfterErasure

  val NoVisitor: Visitor = (_) => ()
  type Transformer = () => (Context => Tree => Tree)
  type Optimization = (Context) => (String, ErasureCompatibility, Visitor, Transformer)

  private lazy val _optimizations: Seq[Optimization] = Seq(
    inlineCaseIntrinsics
    ,inlineOptions
    ,inlineLabelsCalledOnce
    ,valify
    ,devalify
    ,jumpjump
    ,dropGoodCasts
    ,dropNoEffects
    ,inlineLocalObjects // followCases needs to be fixed, see ./tests/pos/rbtree.scala
    /*, varify*/  // varify could stop other transformations from being applied. postponed.
    //, bubbleUpNothing
    ,constantFold
  )

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context, info: TransformerInfo): tpd.Tree = {
    val ctx0 = ctx
    if (optimize && !tree.symbol.is(Flags.Label)) {
      implicit val ctx: Context = ctx0.withOwner(tree.symbol(ctx0))
      // TODO: optimize class bodies before erasure?
      var rhs0 = tree.rhs
      var rhs1: Tree = null
      val erasureCompatibility = if (ctx.erasedTypes) AfterErasure else BeforeErasure
      while (rhs1 ne rhs0) {
        rhs1 = rhs0
        val initialized = _optimizations.map(x =>x(ctx.withOwner(tree.symbol)))
        var (names, erasureSupport , visitors, transformers) = unzip4(initialized)
        // todo: fuse for performance
        while (names.nonEmpty) {
          val nextVisitor = visitors.head
          val supportsErasure = erasureSupport.head
          if ((supportsErasure & erasureCompatibility) > 0) {
            rhs0.foreachSubTree(nextVisitor)
            val nextTransformer = transformers.head()
            val name = names.head
            val rhst = new TreeMap() {
              override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = {
                val innerCtx = if (tree.isDef && tree.symbol.exists) ctx.withOwner(tree.symbol) else ctx
                nextTransformer(ctx)(super.transform(tree)(innerCtx))
              }
            }.transform(rhs0)
//            if (rhst ne rhs0)
//              println(s"${tree.symbol} after ${name} became ${rhst.show}")
            rhs0 = rhst
          }
          names = names.tail
          visitors = visitors.tail
          erasureSupport = erasureSupport.tail
          transformers = transformers.tail
        }
      }
      if (rhs0 ne tree.rhs) cpy.DefDef(tree)(rhs = rhs0)
      else tree
    } else tree
  }

  val inlineCaseIntrinsics: Optimization = { (ctx0: Context) => {
    implicit val ctx: Context = ctx0
    val transformer: Transformer = () => localCtx => {
      case a: Apply if !a.tpe.isInstanceOf[MethodicType] && a.symbol.is(Flags.Synthetic) && a.symbol.owner.is(Flags.Module) &&
        (a.symbol.name == nme.apply) && a.symbol.owner.companionClass.is(Flags.CaseClass) =>
        def unrollArgs(t: Tree, l: List[List[Tree]]): List[List[Tree]] = t match {
          case Apply(t, args) => unrollArgs(t, args :: l)
          case _ => l
        }
        val argss = unrollArgs(a.fun, a.args :: Nil)
        def rollInArgs(l: List[List[Tree]], fun: Tree): Tree = l match {
          case head :: tail => rollInArgs(tail, fun.appliedToArgs(head))
          case _ => fun
        }
        rollInArgs(argss.tail, tpd.New(a.tpe.dealias, argss.head))
      case a: Apply if a.symbol.is(Flags.Synthetic) && a.symbol.owner.is(Flags.Module) &&
        (a.symbol.name == nme.unapply) && a.symbol.owner.companionClass.is(Flags.CaseClass) =>
        if (!a.symbol.owner.is(Flags.Scala2x)) {
          if (a.tpe.derivesFrom(defn.BooleanClass)) Literal(Constant(true))
          else a.args.head
        } else if (a.tpe.derivesFrom(defn.OptionClass) && a.args.head.tpe.derivesFrom(a.symbol.owner.companionClass)) {
          // todo: if args is an expression, this will evaluate it multiple times
          // todo: if the static type is right, it does not mean it's not null
          val accessors = a.args.head.tpe.widenDealias.classSymbol.caseAccessors.filter(_.is(Flags.Method))
          val fields = accessors.map(x => a.args.head.select(x).ensureApplied)
          val tplType = a.tpe.baseArgTypes(defn.OptionClass).head

          if (fields.tail.nonEmpty) {
            val tplAlloc = tpd.New(tplType, fields)
            tpd.New(a.tpe.dealias.translateParameterized(defn.OptionClass, defn.SomeClass), tplAlloc :: Nil)
          } else { // scalac does not have tupple1
            tpd.New(a.tpe.dealias.translateParameterized(defn.OptionClass, defn.SomeClass), fields.head :: Nil)
          }
        }
        else a
      case a: Apply if (a.symbol.name == nme.unapplySeq) && a.symbol.owner.derivesFrom(SeqFactoryClass) && a.symbol.extendedOverriddenSymbols.isEmpty =>
        def reciever(t: Tree): Type = t match {
          case t: Apply => reciever(t.fun)
          case t: TypeApply => reciever(t.fun)
          case t: Ident => desugarIdent(t) match {
            case Some(t) => reciever(t)
            case _ => NoType
          }
          case t: Select =>
            t.qualifier.tpe.widenDealias
        }

        val recv = reciever(a)
        if (recv.typeSymbol.is(Flags.Module))
          tpd.New(a.tpe.dealias.translateParameterized(defn.OptionClass, defn.SomeClass), a.args.head :: Nil)
        else a
      case t => t
    }
    ("inlineCaseIntrinsics", BeforeAndAfterErasure, NoVisitor, transformer)
  }}
  
  val constantFold: Optimization = { (ctx0: Context) => {
    implicit val ctx: Context = ctx0
    def preEval(t: Tree) = {
      if (t.isInstanceOf[Literal] || t.isInstanceOf[CaseDef] || !isPureExpr(t)) t else {
        val s = ConstFold.apply(t)
        if ((s ne null) && s.tpe.isInstanceOf[ConstantType]) {
          val constant = s.tpe.asInstanceOf[ConstantType].value
          Literal(constant)
        } else t
      }
    }

    def isSimilar(t1: Tree, t2: Tree): Boolean = t1 match {
      case t1: Apply =>
        t2 match {
          case t2: Apply =>
            (t1.symbol == t2.symbol) && (t1.args zip t2.args).forall(x => isSimilar(x._1, x._2)) && isSimilar(t1.fun, t2.fun)
          case _ => false
        }
      case t1: Ident =>
        desugarIdent(t1) match {
          case Some(t) =>
            val t2i = t2 match {
              case t2: Ident => desugarIdent(t2).getOrElse(t2)
              case _ => t2
            }
            isSimilar(t, t2i)
          case None => t1.symbol eq t2.symbol
        }
      case t1: Select => t2 match {
        case t2: Select => (t1.symbol eq t2.symbol) && isSimilar(t1.qualifier, t2.qualifier)
        case t2: Ident => desugarIdent(t2) match {
          case Some(t2) => isSimilar(t1, t2)
          case None => false
        }
        case _ => false
      }
      case t1: Literal => t2 match {
        case t2: Literal if t1.const.tag == t2.const.tag && t1.const.value == t2.const.value =>
          true
        case _ => false
      }
      case _ => false
    }

    val transformer: Transformer = () => localCtx => { x => preEval(x) match {
      // TODO: include handling of isInstanceOf similar to one in IsInstanceOfEvaluator
      // TODO: include methods such as Int.int2double(see ./tests/pos/harmonize.scala)
      case If(cond1, thenp, elsep) if isSimilar(thenp, elsep) =>
        Block(cond1 :: Nil, thenp)
      case If(cond1, If(cond2, thenp2, elsep2), elsep1) if isSimilar(elsep1, elsep2) =>
        If(cond1.select(defn.Boolean_&&).appliedTo(cond2), thenp2, elsep1)
      case If(cond1, If(cond2, thenp2, elsep2), elsep1) if isSimilar(elsep1, thenp2) =>
        If(cond1.select(defn.Boolean_!).select(defn.Boolean_||).appliedTo(cond2), elsep1, elsep2)
      case If(cond1, thenp1, If(cond2, thenp2, elsep2)) if isSimilar(thenp1, thenp2) =>
        If(cond1.select(defn.Boolean_||).appliedTo(cond2), thenp1, elsep2)
      case If(cond1, thenp1, If(cond2, thenp2, elsep2)) if isSimilar(thenp1, elsep2) =>
        If(cond1.select(defn.Boolean_||).appliedTo(cond2.select(defn.Boolean_!)), thenp1, thenp2)
      case If(t: Literal, thenp, elsep) =>
        if (t.const.booleanValue) thenp
        else elsep
      case ift @ If(cond, thenp: Literal, elsep: Literal) if ift.tpe.derivesFrom(defn.BooleanClass) && thenp.const.booleanValue && !elsep.const.booleanValue =>
        if (thenp.const.booleanValue && !elsep.const.booleanValue) {
          cond
        }  else if (!thenp.const.booleanValue && elsep.const.booleanValue) {
          cond.select(defn.Boolean_!).ensureApplied
        } else ??? //should never happen becase it would be similar
//         the lower two are disabled, as it may make the isSimilar rule not apply for a nested structure of iffs.
//         see the example below:
    //        (b1, b2) match {
    //          case (true, true) => true
    //          case (false, false) => true
    //          case _ => false
    //        }
//      case ift @ If(cond, thenp: Literal, elsep) if ift.tpe.derivesFrom(defn.BooleanClass) && thenp.const.booleanValue =>
//        //if (thenp.const.booleanValue)
//          cond.select(defn.Boolean_||).appliedTo(elsep)
//        //else // thenp is false, this tree is bigger then the original
//        //  cond.select(defn.Boolean_!).select(defn.Boolean_&&).appliedTo(elsep)
//      case ift @ If(cond, thenp, elsep :Literal) if ift.tpe.derivesFrom(defn.BooleanClass) && !elsep.const.booleanValue =>
//        cond.select(defn.Boolean_&&).appliedTo(elsep)
//        // the other case ins't handled intentionally. See previous case for explanation
      case If(t@ Select(recv, _), thenp, elsep) if t.symbol eq defn.Boolean_! =>
        tpd.If(recv, elsep, thenp)
      case If(t@ Apply(Select(recv, _), Nil), thenp, elsep) if t.symbol eq defn.Boolean_! =>
        tpd.If(recv, elsep, thenp)
      // todo: similar trick for comparisons.
      // todo: handle comparison with min\max values
      case Apply(meth1 @ Select(Apply(meth2 @ Select(rec, _), Nil), _), Nil) if meth1.symbol == defn.Boolean_! && meth2.symbol == defn.Boolean_! =>
        rec
      case meth1 @ Select(meth2 @ Select(rec, _), _) if meth1.symbol == defn.Boolean_! && meth2.symbol == defn.Boolean_! && !ctx.erasedTypes =>
        rec
      case t@Apply(Select(lhs, _), List(rhs)) =>
        val sym = t.symbol
        (lhs, rhs) match {
          case (lhs, Literal(_)) if !lhs.isInstanceOf[Literal] && symmetricOperations.contains(sym) =>
            rhs.select(sym).appliedTo(lhs)
          case (l , _) if (sym == defn.Boolean_&&) && l.tpe.isInstanceOf[ConstantType]  =>
            val const = l.tpe.asInstanceOf[ConstantType].value.booleanValue
            if (const) Block(lhs :: Nil, rhs)
            else l

          case (l, x: Literal) if sym == defn.Boolean_== && l.tpe.derivesFrom(defn.BooleanClass) && x.tpe.derivesFrom(defn.BooleanClass) =>
            if (x.const.booleanValue) l
            else l.select(defn.Boolean_!).ensureApplied
          case (l, x: Literal) if sym == defn.Boolean_!= && l.tpe.derivesFrom(defn.BooleanClass) && x.tpe.derivesFrom(defn.BooleanClass) =>
            if (!x.const.booleanValue) l
            else l.select(defn.Boolean_!).ensureApplied
          case (x: Literal, l) if sym == defn.Boolean_== && l.tpe.derivesFrom(defn.BooleanClass) && x.tpe.derivesFrom(defn.BooleanClass) =>
            if (x.const.booleanValue) l
            else l.select(defn.Boolean_!).ensureApplied
          case (x: Literal, l) if sym == defn.Boolean_!= && l.tpe.derivesFrom(defn.BooleanClass) && x.tpe.derivesFrom(defn.BooleanClass) =>
            if (!x.const.booleanValue) l
            else l.select(defn.Boolean_!).ensureApplied

          case (l: Literal, _) if (sym == defn.Boolean_||) && l.tpe.isInstanceOf[ConstantType]   =>
            val const = l.tpe.asInstanceOf[ConstantType].value.booleanValue
            if (l.const.booleanValue) l
            else Block(lhs :: Nil, rhs)
//          case (Literal(Constant(1)), _)    if sym == defn.Int_*  => rhs
//          case (Literal(Constant(0)), _)    if sym == defn.Int_+  => rhs
//          case (Literal(Constant(1L)), _)   if sym == defn.Long_* => rhs
//          case (Literal(Constant(0L)), _)   if sym == defn.Long_+ => rhs
//          // todo: same for float, double, short
//          // todo: empty string concat
//          // todo: disctribute & reorder constants
//          // todo: merge subsequent casts
//          case (_, Literal(Constant(1)))    if sym == defn.Int_/  => lhs
//          case (_, Literal(Constant(1L)))   if sym == defn.Long_/ => lhs
//          case (_, Literal(Constant(0)))    if sym == defn.Int_/  =>
//            Block(List(lhs),
//              ref(defn.throwMethod).appliedTo(tpd.New(defn.ArithmeticExceptionClass.typeRef, defn.ArithmeticExceptionClass_stringConstructor, Literal(Constant("/ by zero")) :: Nil)))
//          case (_, Literal(Constant(0L)))  if sym == defn.Long_/ =>
//            Block(List(lhs),
//              ref(defn.throwMethod).appliedTo(tpd.New(defn.ArithmeticExceptionClass.typeRef, defn.ArithmeticExceptionClass_stringConstructor, Literal(Constant("/ by zero")) :: Nil)))
          case _ => t
        }
      case t: Match if (t.selector.tpe.isInstanceOf[ConstantType] && t.cases.forall(x => x.pat.tpe.isInstanceOf[ConstantType] || (tpd.isWildcardArg(x.pat) && x.guard.isEmpty))) =>
        val selectorValue = t.selector.tpe.asInstanceOf[ConstantType].value
        val better = t.cases.find(x => tpd.isWildcardArg(x.pat) || (x.pat.tpe.asInstanceOf[ConstantType].value eq selectorValue))
        if (better.nonEmpty) better.get.body
        else t
      case t: Literal =>
        t
      case t: CaseDef =>
        t
      case t if !isPureExpr(t) =>
        t
      case t =>
        val s = ConstFold.apply(t)
        if ((s ne null) && s.tpe.isInstanceOf[ConstantType]) {
          val constant = s.tpe.asInstanceOf[ConstantType].value
          Literal(constant)
        } else t
    }}
    ("constantFold", BeforeAndAfterErasure, NoVisitor, transformer)
  }}

  val inlineLocalObjects: Optimization = { (ctx0: Context) => {
    implicit val ctx: Context = ctx0
    val hasPerfectRHS = collection.mutable.HashMap[Symbol, Boolean]() // in the end only calls constructor. Reason for unconditional inlining
    val checkGood = collection.mutable.HashMap[Symbol, Set[Symbol]]() // if all values have perfect RHS than key has perfect RHS
    val forwarderWritesTo = collection.mutable.HashMap[Symbol, Symbol]()
    val gettersCalled = collection.mutable.HashSet[Symbol]()
    def followTailPerfect(t: Tree, symbol: Symbol): Unit = {
      t match {
        case Block(_, expr) => followTailPerfect(expr, symbol)
        case If(_, thenp, elsep) => followTailPerfect(thenp, symbol); followTailPerfect(elsep, symbol);
        case Apply(fun, _) if fun.symbol.isConstructor && t.tpe.widenDealias == symbol.info.widenDealias.finalResultType.widenDealias =>
          hasPerfectRHS(symbol) = true
        case Apply(fun, _) if fun.symbol.is(Flags.Label) && (fun.symbol ne symbol) =>
          checkGood.put(symbol, checkGood.getOrElse(symbol, Set.empty) + fun.symbol)
          assert(forwarderWritesTo.getOrElse(t.symbol, symbol) == symbol)
          forwarderWritesTo(t.symbol) = symbol
        case t: Ident if !t.symbol.owner.isClass && (t.symbol ne symbol) =>
          checkGood.put(symbol, checkGood.getOrElse(symbol, Set.empty) + t.symbol)
        case _ =>
      }
    }
    val visitor: Visitor = {
      case vdef: ValDef if (vdef.symbol.info.classSymbol is Flags.CaseClass) && !vdef.symbol.is(Flags.Lazy)  && !vdef.symbol.info.classSymbol.caseAccessors.exists(x => x.is(Flags.Mutable)) =>
        followTailPerfect(vdef.rhs, vdef.symbol)
      case Assign(lhs, rhs) if !lhs.symbol.owner.isClass =>
        checkGood.put(lhs.symbol, checkGood.getOrElse(lhs.symbol, Set.empty) + rhs.symbol)
      case t @ Select(qual, _) if (t.symbol.isGetter && !t.symbol.is(Flags.Mutable)) ||
        (t.symbol.maybeOwner.derivesFrom(defn.ProductClass) && t.symbol.maybeOwner.is(Flags.CaseClass) && t.symbol.name.isProductAccessorName) ||
        (t.symbol.is(Flags.CaseAccessor) && !t.symbol.is(Flags.Mutable)) =>
        gettersCalled(qual.symbol) = true
      case t: DefDef if t.symbol.is(Flags.Label) =>
        followTailPerfect(t.rhs, t.symbol)
      case _ =>
    }

    val transformer: Transformer = () => {
      var hasChanged = true
      while(hasChanged) {
        hasChanged = false
        checkGood.foreach{case (key, values) =>
          values.foreach { value =>
            if (hasPerfectRHS.getOrElse(key, false)) {
              hasChanged = !hasPerfectRHS.put(value, true).getOrElse(false)
            }
          }
        }
      }

      val newMappings: Map[Symbol, Map[Symbol, Symbol]] =
        hasPerfectRHS.iterator.map(x => x._1).filter(x => !x.is(Flags.Method) && !x.is(Flags.Label) && gettersCalled.contains(x.symbol) && (x.symbol.info.classSymbol is Flags.CaseClass))
          .map{ refVal =>
//          println(s"replacing ${refVal.symbol.fullName} with stack-allocated fields")
          var accessors = refVal.info.classSymbol.caseAccessors.filter(_.isGetter) // todo: drop mutable ones
          if (accessors.isEmpty) accessors = refVal.info.classSymbol.caseAccessors
          val productAccessors = (1 to accessors.length).map(i => refVal.info.member(nme.productAccessorName(i)).symbol) // todo: disambiguate
          val newLocals = accessors.map(x =>
            // todo: it would be nice to have an additional optimization that
            // todo: is capable of turning those mutable ones into immutable in common cases
            ctx.newSymbol(ctx.owner.enclosingMethod, (refVal.name + "$" + x.name).toTermName, Flags.Synthetic | Flags.Mutable, x.asSeenFrom(refVal.info).info.finalResultType.widenDealias)
          )
            val fieldMapping = accessors zip newLocals
            val productMappings = productAccessors zip newLocals
            (refVal, (fieldMapping ++ productMappings).toMap)
          }.toMap
      val toSplit: mutable.Set[Symbol] = mutable.Set.empty ++ newMappings.keySet

      def splitWrites(t: Tree, target: Symbol): Tree = {
        t match {
          case tree@ Block(stats, expr) => cpy.Block(tree)(stats, splitWrites(expr, target))
          case tree@ If(_, thenp, elsep) => cpy.If(tree)(thenp = splitWrites(thenp, target), elsep =  splitWrites(elsep, target))
          case Apply(sel , args) if sel.symbol.isConstructor && t.tpe.widenDealias == target.info.widenDealias.finalResultType.widenDealias =>
            val fieldsByAccessors = newMappings(target)
            var accessors = target.info.classSymbol.caseAccessors.filter(_.isGetter)  // todo: when is this filter needed?
            if (accessors.isEmpty) accessors = target.info.classSymbol.caseAccessors
            val assigns = (accessors zip args) map (x => ref(fieldsByAccessors(x._1)).becomes(x._2))
            val recreate = sel.appliedToArgs(accessors.map(x => ref(fieldsByAccessors(x))))
            Block(assigns, recreate)
          case Apply(fun, _) if fun.symbol.is(Flags.Label) =>
            t // do nothing. it will do on its own
          case t: Ident if !t.symbol.owner.isClass && newMappings.contains(t.symbol) && t.symbol.info.classSymbol == target.info.classSymbol =>
            val fieldsByAccessorslhs = newMappings(target)
            val fieldsByAccessorsrhs = newMappings(t.symbol)
            val accessors = target.info.classSymbol.caseAccessors.filter(_.isGetter)
            val assigns = accessors map (x => ref(fieldsByAccessorslhs(x)).becomes(ref(fieldsByAccessorsrhs(x))))
            Block(assigns, t)
          // if t is itself split, push writes
          case _ =>
            evalOnce(t){ev =>
              if (ev.tpe.derivesFrom(defn.NothingClass)) ev
              else {
                val fieldsByAccessors = newMappings(target)
                val accessors = target.info.classSymbol.caseAccessors.filter(_.isGetter)
                val assigns = accessors map (x => ref(fieldsByAccessors(x)).becomes(ev.select(x)))
                Block(assigns, ev)
              }
            } // need to eval-once and update fields

        }
      }

      def followCases(t: Symbol): Symbol = if (t.symbol.is(Flags.Label)) {
        // todo: this can create cycles, see ./tests/pos/rbtree.scala
        followCases(forwarderWritesTo.getOrElse(t.symbol, NoSymbol))
      } else t

      hasPerfectRHS.clear()
      //checkGood.clear()
      gettersCalled.clear()

      val res: Context => Tree => Tree = {localCtx => (t: Tree) => t match {
        case ddef: DefDef if ddef.symbol.is(Flags.Label) =>
          newMappings.get(followCases(ddef.symbol)) match {
            case Some(mappings) =>
              cpy.DefDef(ddef)(rhs = splitWrites(ddef.rhs, followCases(ddef.symbol)))
            case _ => ddef
          }
        case a: ValDef if toSplit.contains(a.symbol) =>
          toSplit -= a.symbol
          // break ValDef apart into fields + boxed value
          val newFields = newMappings(a.symbol).values.toSet
          Thicket(
            newFields.map(x => tpd.ValDef(x.asTerm, tpd.defaultValue(x.symbol.info.widenDealias))).toList :::
              List(cpy.ValDef(a)(rhs = splitWrites(a.rhs, a.symbol))))
        case ass: Assign =>
          newMappings.get(ass.lhs.symbol) match {
            case None =>   ass
            case Some(mapping) =>
              val updates = mapping.filter(x => x._1.is(Flags.CaseAccessor)).map(x => ref(x._2).becomes(ref(ass.lhs.symbol).select(x._1))).toList
              Thicket(ass :: updates)
          }
        case sel @ Select(rec, _) if (t.symbol.isGetter && !t.symbol.is(Flags.Mutable)) ||
          (t.symbol.maybeOwner.derivesFrom(defn.ProductClass) && t.symbol.owner.is(Flags.CaseClass) && t.symbol.name.isProductAccessorName) ||
          (t.symbol.is(Flags.CaseAccessor) && !t.symbol.is(Flags.Mutable)) =>
          newMappings.getOrElse(rec.symbol, Map.empty).get(sel.symbol) match {
            case None => t
            case Some(newSym) => ref(newSym)
          }
        case t => t
      }}

    res
    }
    ("inlineLocalObjects", BeforeAndAfterErasure, visitor, transformer)
  }}

  private def collectTypeTests(t: Tree)(implicit ctx: Context): List[(Symbol, Type)] = {
    def recur(t: Tree): List[(Symbol, Type)] =
      t match {
        case Apply(x, _) if (x.symbol == defn.Boolean_! || x.symbol == defn.Boolean_||) => List.empty
        case Apply(fun @ Select(x, _), y) if (fun.symbol == defn.Boolean_&&) => recur(x) ++ recur(y.head)
        case TypeApply(fun @Select(x, _), List(tp)) if fun.symbol eq defn.Any_isInstanceOf =>
          if (x.symbol.exists && !x.symbol.owner.isClass && !x.symbol.is(Flags.Method|Flags.Mutable))
            (x.symbol, tp.tpe) :: Nil
          else Nil
        case _ => List.empty
      }
    recur(t)
  }

  val dropGoodCasts: Optimization = { (ctx0: Context) => {
    implicit val ctx: Context = ctx0

    val transformer: Transformer = () => localCtx => {
      case t @ If(cond, thenp, elsep) =>
        val newTested = collectTypeTests(cond)
        val testedMap = newTested.foldRight[Map[Symbol, List[Type]]](Map.empty)((x, y) =>
          y + ((x._1, x._2 :: y.getOrElse(x._1, Nil)))
        )
        val dropGoodCastsInStats = new TreeMap() {
          override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = super.transform(tree) match {
            case t: Block =>
              val nstats = t.stats.filterConserve({
                 case TypeApply(fun @ Select(rec, _), List(tp)) if fun.symbol == defn.Any_asInstanceOf =>
                   !testedMap.getOrElse(rec.symbol, Nil).exists(x => x <:< tp.tpe)
                 case _ => true
              })
              if (nstats eq t.stats) t
              else Block(nstats, t.expr)
            case t => t
          }
        }
        val nthenp = dropGoodCastsInStats.transform(thenp)

        cpy.If(t)(thenp = nthenp, elsep = elsep)
      case t => t
    }
    ("dropGoodCasts", BeforeAndAfterErasure, NoVisitor, transformer)
  }}

  private def keepOnlySideEffects(t: Tree)(implicit ctx: Context): Tree = {
    t match {
      case t: Literal => EmptyTree
      case Typed(exp, tpe) =>
        keepOnlySideEffects(exp)
      case t @ If(cond, thenp, elsep) =>
        val nthenp = keepOnlySideEffects(thenp)
        val nelsep = keepOnlySideEffects(elsep)
        if (thenp.isEmpty && elsep.isEmpty) keepOnlySideEffects(cond)
        else cpy.If(t)(
          thenp = nthenp.orElse(if (thenp.isInstanceOf[Literal]) thenp else  tpd.unitLiteral),
          elsep = nelsep.orElse(if (elsep.isInstanceOf[Literal]) elsep else  tpd.unitLiteral))
      case Select(rec, _) if (t.symbol.isGetter && !t.symbol.is(Flags.Mutable)) ||
        (t.symbol.owner.derivesFrom(defn.ProductClass) && t.symbol.owner.is(Flags.CaseClass) && t.symbol.name.isProductAccessorName) ||
        (t.symbol.is(Flags.CaseAccessor) && !t.symbol.is(Flags.Mutable))=>
        keepOnlySideEffects(rec) // accessing a field of a product
      case Select(qual, _) if !t.symbol.is(Flags.Mutable | Flags.Lazy) && (!t.symbol.is(Flags.Method) || t.symbol.isGetter) =>
        keepOnlySideEffects(qual)
      case Block(List(t: DefDef), s: Closure) => EmptyTree
      case bl@Block(stats, expr) =>
        val stats1 = stats.mapConserve(keepOnlySideEffects)
        val stats2 = if (stats1 ne stats) stats1.filter(x=>x ne EmptyTree) else stats1
        val expr2: tpd.Tree = expr match {
          case t: Literal if t.tpe.derivesFrom(defn.UnitClass) => expr
          case _ => keepOnlySideEffects(expr).orElse(unitLiteral)
        }
        cpy.Block(bl)(stats2, expr2)
      case t: Ident if !t.symbol.is(Flags.Method | Flags.Lazy) && !t.symbol.info.isInstanceOf[ExprType] =>
        desugarIdent(t) match {
          case Some(t) => t
          case None => EmptyTree
        }
      case app: Apply if app.fun.symbol.is(Flags.Label) && !app.tpe.finalResultType.derivesFrom(defn.UnitClass) =>
        val denot = app.fun.symbol.denot
        //println(s"replacing ${app.symbol}")
        if (!denot.info.finalResultType.derivesFrom(defn.UnitClass)) {
          val newLabelType = app.symbol.info match {
            case mt: MethodType =>
              mt.derivedMethodType(mt.paramNames, mt.paramTypes, defn.UnitType)
            case et: ExprType =>
              et.derivedExprType(defn.UnitType)
          }
          val newD = app.symbol.asSymDenotation.copySymDenotation(info = newLabelType)
          newD.installAfter(this)
        }

        ref(app.symbol).appliedToArgs(app.args)
      case t @ Apply(fun, _) if Analysis.effectsDontEscape(t) =>
        def getArgsss(a: Tree): List[Tree] = a match {
          case a: Apply => getArgsss(a.fun) ::: a.args
          case _ => Nil
        }
        def getSel(t: Tree): Tree = {t match {
          case t: Apply => getSel(t.fun)
          case t: Select => t.qualifier
          case t: TypeApply => getSel(t.fun)
          case _ => t
        }}
        val args = getArgsss(t)
        val rec = getSel(t)
        val prefix = rec match {
          case t: New =>
            args.map(keepOnlySideEffects)
          case _ =>
            rec :: args.map(keepOnlySideEffects)
        }
        Block(prefix, tpd.unitLiteral)
      case t @ TypeApply(Select(rec, _), List(testType)) if t.symbol.eq(defn.Any_asInstanceOf) && testType.tpe.widenDealias.typeSymbol.exists =>
        val receiverType = TypeErasure.erasure(rec.tpe)
        val erazedTestedType = TypeErasure.erasure(testType.tpe)
        if (receiverType.derivesFrom(erazedTestedType.typeSymbol))
          EmptyTree
        else t
      case _ => t
    }
  }

  val removeUnnecessaryNullChecks: Optimization = { (ctx0: Context) => {
    implicit val ctx = ctx0
    val initializedVals = mutable.HashSet[Symbol]()
    val visitor: Visitor = {
      case vd: ValDef =>
        val rhs = vd.rhs
        val rhsName = rhs.symbol.name
        if (!vd.symbol.is(Flags.Mutable) &&
          !rhs.isEmpty || rhsName != nme.WILDCARD || rhsName !=
          nme.???) {
          initializedVals += vd.symbol
        }
      case t: Tree =>
    }
    @inline def isLhsNullLiteral(t: Tree) = t match {
      case Select(literalLhs: Literal, _) =>
        literalLhs.const.tag == Constants.NullTag
      case _ => false
    }
    @inline def isRhsNullLiteral(args: List[Tree]) = args match {
      case List(booleanRhs: Literal) =>
        booleanRhs.const.tag == Constants.NullTag
      case _ => false
    }
    val transformer: Transformer = () => localCtx0 => {
      implicit val localCtx = localCtx0
      val transformation: Tree => Tree = {
        case potentialCheck: Apply =>
          val sym = potentialCheck.symbol
          if (isLhsNullLiteral(potentialCheck.fun)) {
            if (sym == defn.Boolean_==) tpd.Literal(Constant(true))
            else if(sym == defn.Boolean_!=) tpd.Literal(Constant(false))
            else potentialCheck
          } else if (isRhsNullLiteral(potentialCheck.args)) {
            if (sym == defn.Boolean_==) tpd.Literal(Constant(true))
            else if(sym == defn.Boolean_!=) tpd.Literal(Constant(false))
            else potentialCheck
          } else potentialCheck
        case t => t
      }
      transformation
    }
    ("removeUnnecessaryNullChecks", BeforeAndAfterErasure, visitor,
      transformer)
  }}

  val bubbleUpNothing: Optimization = { (ctx0: Context) => {
    implicit val ctx: Context = ctx0
    val transformer: Transformer = () => localCtx => {
      case Apply(Select(qual, _), args)  if qual.tpe.derivesFrom(defn.NothingClass) => qual
      case Apply(Select(qual, _), args)  if args.exists(x => x.tpe.derivesFrom(defn.NothingClass)) =>
        val (keep, noth :: other) = args.span(x => !x.tpe.derivesFrom(defn.NothingClass))
        Block(qual :: keep, noth)
      case Assign(_, rhs) if rhs.tpe.derivesFrom(defn.NothingClass) =>
        rhs
      case If(cond, _, _) if cond.tpe.derivesFrom(defn.NothingClass) => cond
      case a: Block if a.stats.exists(x => x.tpe.derivesFrom(defn.NothingClass))  =>
        val (keep, noth :: other) = a.stats.span(x => !x.tpe.derivesFrom(defn.NothingClass))
        val keep2 = other.filter(x => x.isDef)
        Block(keep ::: keep2, noth)
      case t => t
    }
    ("bubbleUpNothing", BeforeAndAfterErasure, NoVisitor, transformer)
  }}

  val dropNoEffects: Optimization = { (ctx0: Context) => {
    implicit val ctx: Context = ctx0

    val transformer: Transformer = () => localCtx => {
      case Block(Nil, expr) => expr
      case a: Block  =>
        val newStats0 = a.stats.mapConserve(keepOnlySideEffects)
        val newStats1 = if (newStats0 eq a.stats) newStats0 else newStats0.flatMap{
          case x: Block=> x.stats ::: List(x.expr)
          case EmptyTree => Nil
          case t => t :: Nil
        }
        val (newStats2, newExpr) = a.expr match {
          case Block(stats2, expr) => (newStats1 ++ stats2, expr)
          case _ => (newStats1, a.expr)
        }
        if (newStats2.nonEmpty)
          cpy.Block(a)(stats = newStats2, newExpr)
        else newExpr
      case a: DefDef =>
        if (a.symbol.info.finalResultType.derivesFrom(defn.UnitClass) && !a.rhs.tpe.derivesFrom(defn.UnitClass) && !a.rhs.tpe.derivesFrom(defn.NothingClass)) {
          def insertUnit(t: Tree) = {
            if (!t.tpe.derivesFrom(defn.UnitClass)) Block(t :: Nil, tpd.unitLiteral)
            else t
          }
          cpy.DefDef(a)(rhs = insertUnit(keepOnlySideEffects(a.rhs)), tpt = tpd.TypeTree(defn.UnitType))
        } else a
      case t => t
    }
    ("dropNoEffects", BeforeAndAfterErasure, NoVisitor, transformer)
  }}

  val inlineLabelsCalledOnce: Optimization = { (ctx0: Context) => {
    implicit val ctx: Context = ctx0
    val timesUsed = collection.mutable.HashMap[Symbol, Int]()
    val defined = collection.mutable.HashMap[Symbol, DefDef]()

    val visitor: Visitor = {
      case defdef: DefDef if defdef.symbol.is(Flags.Label)  =>
        var isRecursive = false
        defdef.rhs.foreachSubTree(x => if (x.symbol == defdef.symbol) isRecursive = true)
        if (!isRecursive) defined.put(defdef.symbol, defdef)
      case t: Apply if t.symbol.is(Flags.Label) =>
        val b4 = timesUsed.getOrElseUpdate(t.symbol, 0)
        timesUsed.put(t.symbol, b4 + 1)
      case _ =>
    }

    val transformer: Transformer = () => localCtx => {
      case a: Apply =>
        defined.get(a.symbol) match {
          case None => a
          case Some(defDef) if a.symbol.is(Flags.Label) && timesUsed.getOrElse(a.symbol, 0) == 1 && a.symbol.info.paramTypess == List(Nil)=>
            //println(s"Inlining ${defDef.name}")
            defDef.rhs.changeOwner(defDef.symbol, localCtx.owner)
          case Some(defDef) if defDef.rhs.isInstanceOf[Literal] =>
            defDef.rhs
          case Some(_) =>
            a
        }
      case a: DefDef if (a.symbol.is(Flags.Label) && timesUsed.getOrElse(a.symbol, 0) == 1 && defined.contains(a.symbol)) =>
        //println(s"Dropping ${a.name} ${timesUsed.get(a.symbol)}")
        defined.put(a.symbol, a)
        EmptyTree
      case a: DefDef if (a.symbol.is(Flags.Label) && timesUsed.getOrElse(a.symbol, 0) == 0 && defined.contains(a.symbol)) =>
        //println(s"Dropping ${a.name} ${timesUsed.get(a.symbol)}")
        EmptyTree
      case t => t
    }
    ("inlineLabelsCalledOnce", BeforeAndAfterErasure, visitor, transformer)
  }}

  val jumpjump: Optimization = { (ctx0: Context) => {
    // optimize label defs that call other label-defs
    implicit val ctx: Context = ctx0
    val defined = collection.mutable.HashMap[Symbol, Symbol]()

    val visitor: Visitor = {
      case defdef: DefDef if defdef.symbol.is(Flags.Label)  =>
        defdef.rhs match {
          case Apply(t, args) if t.symbol.is(Flags.Label) &&
            TypeErasure.erasure(defdef.symbol.info.finalResultType).classSymbol == TypeErasure.erasure(t.symbol.info.finalResultType).classSymbol
            && args.size == defdef.vparamss.map(_.size).sum && (args zip defdef.vparamss.flatten).forall(x => x._1.symbol eq x._2.symbol)  && !(defdef.symbol eq t.symbol) =>
              defined(defdef.symbol) = t.symbol
          case _ =>
        }
      case _ =>
    }

    val transformer: Transformer = () => localCtx => {
      case a: Apply if  defined.contains(a.fun.symbol)=>
        defined.get(a.symbol) match {
          case None => a
          case Some(fwd) =>
            ref(fwd).appliedToArgs(a.args)
        }
      case a: DefDef if defined.contains(a.symbol) =>
        //println(s"dropping ${a.symbol.showFullName} as forwarder to ${defined(a.symbol).showFullName}")
        EmptyTree
      case t => t
    }
    ("jumpjump", BeforeAndAfterErasure, visitor, transformer)
  }}

  val inlineOptions: Optimization = { (ctx0: Context) => {
    implicit val ctx: Context = ctx0
    val somes = collection.mutable.HashMap[Symbol, Tree]()
    val nones = collection.mutable.HashSet[Symbol]()

    val visitor: Visitor = {
      case valdef: ValDef if !valdef.symbol.is(Flags.Mutable) &&
        valdef.rhs.isInstanceOf[Apply] && valdef.rhs.tpe.derivesFrom(defn.SomeClass) &&
        valdef.rhs.symbol.isPrimaryConstructor =>
        val Apply(_, value) = valdef.rhs
        somes(valdef.symbol) = value.head

      case valdef: ValDef if !valdef.symbol.is(Flags.Mutable) &&
        valdef.rhs.isInstanceOf[Apply] && valdef.rhs.tpe.derivesFrom(defn.NoneClass)  =>
        nones += valdef.symbol
      case _ =>
    }

    val transformer: Transformer = () => localCtx => tree => {
      def rewriteSelect(x: Tree) = x match {
        case Select(rec, nm) if nm == nme.get && somes.contains(rec.symbol) =>
          somes(rec.symbol)
        case Select(rec, nm) if nm == nme.isDefined &&
          (/*rec.tpe.derivesFrom(defn.SomeClass) ||*/ somes.contains(rec.symbol)) =>
          Literal(Constant(true))
        case Select(rec, nm) if nm == nme.isEmpty &&
          (/*rec.tpe.derivesFrom(defn.SomeClass) ||*/ somes.contains(rec.symbol)) =>
          Literal(Constant(false))

        case Select(rec, nm) if nm == nme.get && nones.contains(rec.symbol) =>
          ref(defn.NoneModuleRef)
        case Select(rec, nm) if nm == nme.isDefined &&
          (/*rec.tpe.derivesFrom(defn.NoneClass) || */ nones.contains(rec.symbol)) =>
          Literal(Constant(false))
        case Select(rec, nm) if nm == nme.isEmpty &&
          (/*rec.tpe.derivesFrom(defn.NoneClass) ||*/ nones.contains(rec.symbol)) =>
          Literal(Constant(true))
        case t => t
      }
      def dropApply(a: Tree): Tree = a match {
        case Apply(fun, Nil) => fun
        case _ => a
      }
      val old = dropApply(tree)
      val nw = rewriteSelect(old)
      if (nw ne old) nw
      else tree
    }
    ("inlineLabelsCalledOnce", BeforeAndAfterErasure, visitor, transformer)
  }}

  val valify: Optimization = { (ctx0: Context) => {
    implicit val ctx: Context = ctx0
     // either a duplicate or a read through series of immutable fields
    val defined: mutable.Map[Symbol, ValDef] = mutable.Map()
    val firstRead: mutable.Map[Symbol, RefTree] = mutable.Map()
    val firstWrite: mutable.Map[Symbol, Assign] = mutable.Map()
    val secondWrite: mutable.Map[Symbol, Assign] = mutable.Map()
    val visitor: Visitor = {
      case t: ValDef if t.symbol.is(Flags.Mutable, Flags.Lazy) && !t.symbol.is(Flags.Method) && !t.symbol.owner.isClass =>
        if (tpd.isPureExpr(t.rhs))
          defined(t.symbol) = t
      case t: RefTree if !t.symbol.is(Flags.Method) && !t.symbol.owner.isClass =>
        if (!firstWrite.contains(t.symbol)) firstRead(t.symbol) = t
      case t @ Assign(l, expr) if  !l.symbol.is(Flags.Method) && !l.symbol.owner.isClass =>
        if (!firstRead.contains(l.symbol)) {
          if (firstWrite.contains(l.symbol)) {
            if (!secondWrite.contains(l.symbol))
              secondWrite(l.symbol) = t
          } else if (!expr.existsSubTree(x => x match {
            case tree: RefTree if x.symbol == l.symbol => firstRead(l.symbol) = tree; true
            case _ => false
          })) {
            firstWrite(l.symbol) = t
          }
        }
      case _ =>
    }

    val transformer: Transformer = () => localCtx => {
      val transformation: Tree => Tree = {
        case t: Block => // drop non-side-effecting stats
          val valdefs = t.stats.filter(x => x match  {
            case t: ValDef if defined.contains(t.symbol) => true
            case _ => false
          }).asInstanceOf[List[ValDef]]
          val assigns = t.stats.filter(x => x match {
            case t @ Assign(lhs, r) =>
              firstWrite.contains(lhs.symbol) && !secondWrite.contains(lhs.symbol)
            case _ => false
          })

          val pairs = valdefs.flatMap(x => assigns.find(y => y.asInstanceOf[Assign].lhs.symbol == x.symbol) match {
            case Some(y: Assign) => List((x, y))
            case _ => Nil
          })

          val valsToDrop = pairs.map(x => x._1).toSet
          val assignsToReplace: Map[Assign, ValDef] = pairs.map(x => (x._2, x._1)).toMap

          val newStats = t.stats.mapConserve {
            case x: ValDef if valsToDrop.contains(x) => EmptyTree
            case t: Assign => assignsToReplace.get(t) match {
              case Some(vd) =>
                val newD = vd.symbol.asSymDenotation.copySymDenotation(initFlags = vd.symbol.flags.&~(Flags.Mutable))
                newD.installAfter(this)
                tpd.ValDef(vd.symbol.asTerm, t.rhs)
              case None => t
            }
            case x => x
          }

          if (newStats eq t.stats) t
          else cpy.Block(t)(newStats, t.expr)
        case tree => tree
      }

      transformation
    }
    ("valify", BeforeAndAfterErasure, visitor, transformer)
  }}

  val devalify: Optimization = { (ctx0: Context) => {
    implicit val ctx: Context = ctx0
    val timesUsed = collection.mutable.HashMap[Symbol, Int]()
    val defined = collection.mutable.HashSet[Symbol]()
    val copies = collection.mutable.HashMap[Symbol, Tree]() // either a duplicate or a read through series of immutable fields
    val visitor: Visitor = {
      case valdef: ValDef if !valdef.symbol.is(Flags.Param) && !valdef.symbol.is(Flags.Mutable | Flags.Module | Flags.Lazy) && (valdef.symbol.exists && !valdef.symbol.owner.isClass) =>
        defined += valdef.symbol

        dropCasts(valdef.rhs) match {
          case t: Tree if readingOnlyVals(t) =>
            copies.put(valdef.symbol, valdef.rhs)
          case _ =>
        }
      case t: New =>
        val symIfExists = t.tpt.tpe.normalizedPrefix.termSymbol
        val b4 = timesUsed.getOrElseUpdate(symIfExists, 0)
        timesUsed.put(symIfExists, b4 + 1)

      case valdef: ValDef  if valdef.symbol.exists && !valdef.symbol.owner.isClass && !valdef.symbol.is(Flags.Param | Flags.Module | Flags.Lazy) =>
        //todo: handle params after constructors. Start changing public signatures by eliminating unused arguments.
        defined += valdef.symbol

      case t: RefTree =>
        val b4 = timesUsed.getOrElseUpdate(t.symbol, 0)
        timesUsed.put(t.symbol, b4 + 1)
      case _ =>
    }

    val transformer: Transformer = () => localCtx => {

      val valsToDrop = defined -- timesUsed.keySet
      val copiesToReplaceAsDuplicates = copies.filter { x =>
        val rhs = dropCasts(x._2)
        rhs.isInstanceOf[Literal] || (!rhs.symbol.owner.isClass && !rhs.symbol.is(Flags.Method) && !rhs.symbol.is(Flags.Mutable))
      }
      // todo: if a non-synthetic val is duplicate of a synthetic one, rename a synthetic one and drop synthetic flag?

      val copiesToReplaceAsUsedOnce =
        timesUsed.filter(x => x._2 == 1).
          flatMap(x => copies.get(x._1) match {
            case Some(tr) => List((x._1, tr));
            case None => Nil
          })


      val replacements = copiesToReplaceAsDuplicates ++ copiesToReplaceAsUsedOnce

      val deepReplacer = new TreeMap() {
        override def transform(tree: tpd.Tree)(implicit ctx: Context): tpd.Tree = {
          def loop(tree: Tree):Tree  =
            tree match {
              case t: RefTree if replacements.contains(t.symbol) =>
                loop(replacements(t.symbol))
              case _ => tree
            }
          super.transform(loop(tree))
        }
      }

      val transformation: Tree => Tree = {
        case t: ValDef if valsToDrop.contains(t.symbol) =>
          //println(s"droping definition of ${t.symbol.showFullName} as not used")
          t.rhs.changeOwner(t.symbol, t.symbol.owner)
        case t: ValDef if replacements.contains(t.symbol) =>
          //println(s"droping definition of ${t.symbol.showFullName} as an alias")
          EmptyTree
        case t: Block => // drop non-side-effecting stats
          t
        case t: New =>
          val symIfExists = t.tpt.tpe.normalizedPrefix.termSymbol
          if (replacements.contains(symIfExists)) {
            val newPrefix = deepReplacer.transform(replacements(symIfExists))
            val newTpt = t.tpt.tpe match {
              case t: NamedType =>
                t.derivedSelect(newPrefix.tpe)
            }
            tpd.New(newTpt)
          }
          else t
        case t: RefTree if !t.symbol.is(Flags.Method) && !t.symbol.is(Flags.Param) && !t.symbol.is(Flags.Mutable) =>
          if (replacements.contains(t.symbol))
            deepReplacer.transform(replacements(t.symbol)).ensureConforms(t.tpe.widen)
          else t
        case tree => tree
      }

      transformation
    }
    ("devalify", BeforeAndAfterErasure, visitor, transformer)
  }}

  val varify: Optimization = { (ctx0: Context) => {
    implicit val ctx: Context = ctx0
    val paramsTimesUsed = collection.mutable.HashMap[Symbol, Int]()
    val possibleRenames = collection.mutable.HashMap[Symbol, Set[Symbol]]()
    val visitor: Visitor = {
      case t: ValDef
        if t.symbol.is(Flags.Param) =>
        paramsTimesUsed += (t.symbol -> 0)
      case valDef: ValDef
        if valDef.symbol.is(Flags.Mutable) =>
        valDef.rhs.foreachSubTree { subtree =>
          if (paramsTimesUsed.contains(subtree.symbol) &&
            valDef.symbol.info.widenDealias <:< subtree.symbol.info.widenDealias) {
            val newSet = possibleRenames.getOrElse(valDef.symbol, Set.empty) + subtree.symbol
            possibleRenames.put(valDef.symbol, newSet)
          }
        }
      case t: RefTree
        if paramsTimesUsed.contains(t.symbol) =>
        val param = t.symbol
        val current = paramsTimesUsed.get(param)
        current foreach { c => paramsTimesUsed += (param -> (c + 1)) }
      case _ =>
    }
    val transformer: Transformer = () => localCtx => {
      val paramCandidates = paramsTimesUsed.filter(kv => kv._2 == 1).keySet
      val renames = possibleRenames.iterator.map(kv => (kv._1, kv._2.intersect(paramCandidates))).
        filter(x => x._2.nonEmpty).map(x => (x._1, x._2.head)).toMap
      val transformation: Tree => Tree = {
        case t: RefTree
          if renames.contains(t.symbol) =>
          ref(renames(t.symbol))
        case t: ValDef
          if renames.contains(t.symbol) =>
          val replaced = renames(t.symbol)
          if (t.rhs.symbol == replaced) EmptyTree
          else ref(replaced).becomes(t.rhs)
        case t: ValDef
          if paramCandidates.contains(t.symbol) =>
          t.symbol.flags = Flags.Mutable
          t
        case t => t
      }
      transformation
    }
    ("varify", AfterErasure, visitor, transformer)
  }}


  private def unzip4[A, B, C, D](seq: Seq[(A, B, C, D)]): (Seq[A], Seq[B], Seq[C], Seq[D]) = {
    val listBuilderA = new ListBuffer[A]()
    val listBuilderB = new ListBuffer[B]()
    val listBuilderC = new ListBuffer[C]()
    val listBuilderD = new ListBuffer[D]()
    seq.foreach{x =>
      listBuilderA += x._1
      listBuilderB += x._2
      listBuilderC += x._3
      listBuilderD += x._4
    }
    (listBuilderA.toList, listBuilderB.toList, listBuilderC.toList, listBuilderD.toList)
  }
}
