package dotty.tools.dotc
package transform.linker

import core._
import core.Constants.Constant
import core.Contexts._
import core.Decorators._
import core.DenotTransformers.IdentityDenotTransformer
import core.NameOps._
import core.StdNames._
import core.Symbols._
import core.Types._
import ast.Trees._
import ast.tpd
import scala.collection.mutable
import transform.SymUtils._
import transform.TreeTransforms.{MiniPhaseTransform, TransformerInfo, TreeTransform}
import typer.ConstFold
import dotty.tools.dotc.config.Printers.simplify
import Flags._

/** This phase consists of a series of small, simple, local optimizations
 *  applied as a fix point transformation over Dotty Trees.
 *
 *  The termination condition uses referential equality on Trees. Furthermore,
 *  termination relies of every optimization to be shrinking transformations.
 */
class Simplify extends MiniPhaseTransform with IdentityDenotTransformer {
  import tpd._

  override def phaseName: String = "simplify"

  private var SeqFactoryClass: Symbol = null
  private var symmetricOperations: Set[Symbol] = null
  var optimize = false


  override val cpy = tpd.cpy

  override def prepareForUnit(tree: Tree)(implicit ctx: Context): TreeTransform = {
    SeqFactoryClass = ctx.requiredClass("scala.collection.generic.SeqFactory")
    symmetricOperations = Set(defn.Boolean_&&, defn.Boolean_||, defn.Int_+, defn.Int_*, defn.Long_+, defn.Long_*)
    optimize = ctx.settings.optimise.value
    this
  }

  type Visitor = Tree => Unit
  type ErasureCompatibility = Int
  val BeforeErasure: ErasureCompatibility = 1
  val AfterErasure: ErasureCompatibility  = 2
  val BeforeAndAfterErasure: ErasureCompatibility  = BeforeErasure | AfterErasure

  val NoVisitor: Visitor = (_) => ()
  type Transformer = () => (Context => Tree => Tree)

  /** Every optimization is a function of the following type.
   *
   *  - String is the optimization name (for debugging)
   *
   *  - ErasureCompatibility is flag indicating whether this optimization can
   *    be run before or after Erasure (or both).
   *
   *  - Visitor is run first to gather information on Trees (using mutation)
   *
   *  - Transformer does the actual Tree => Tree transformation, possibly
   *  - using a different context from the one using in Optimization.
   */
  type Optimization = (Context) => (String, ErasureCompatibility, Visitor, Transformer)

  private lazy val _optimizations: Seq[Optimization] =
    inlineCaseIntrinsics        ::
    removeUnnecessaryNullChecks ::
    inlineOptions               ::
    inlineLabelsCalledOnce      ::
    valify                      ::
    devalify                    ::
    jumpjump                    ::
    dropGoodCasts               ::
    dropNoEffects               ::
    // inlineLocalObjects          :: // followCases needs to be fixed, see ./tests/pos/rbtree.scala
    // varify                      :: // varify could stop other transformations from being applied. postponed.
    // bubbleUpNothing             ::
    constantFold                ::
    Nil

  // The entry point of local optimisation: DefDefs
  override def transformDefDef(tree: DefDef)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val ctx0 = ctx
    if (optimize && !tree.symbol.is(Label)) {
      implicit val ctx: Context = ctx0.withOwner(tree.symbol(ctx0))
      // TODO: optimize class bodies before erasure?
      var rhs0 = tree.rhs
      var rhs1: Tree = null
      val erasureCompatibility = if (ctx.erasedTypes) AfterErasure else BeforeErasure
      while (rhs1 ne rhs0) {
        rhs1 = rhs0
        val initialized = _optimizations.map(x => x(ctx.withOwner(tree.symbol)))
        var (names, erasureSupport , visitors, transformers) = unzip4(initialized)
        // TODO: fuse for performance
        while (names.nonEmpty) {
          val nextVisitor = visitors.head
          val supportsErasure = erasureSupport.head
          if ((supportsErasure & erasureCompatibility) > 0) {
            rhs0.foreachSubTree(nextVisitor)
            val nextTransformer = transformers.head()
            val name = names.head
            val rhst = new TreeMap() {
              override def transform(tree: Tree)(implicit ctx: Context): Tree = {
                val innerCtx = if (tree.isDef && tree.symbol.exists) ctx.withOwner(tree.symbol) else ctx
                nextTransformer(ctx)(super.transform(tree)(innerCtx))
              }
            }.transform(rhs0)
            if (rhst ne rhs0) {
              simplify.println(s"${tree.symbol} was simplified by ${name}: ${rhs0.show}")
              simplify.println(s"became: ${rhst.show}")
            }
            rhs0 = rhst
          }
          names = names.tail
          visitors = visitors.tail
          erasureSupport = erasureSupport.tail
          transformers = transformers.tail
        }
      }
      if (rhs0 ne tree.rhs) tpd.cpy.DefDef(tree)(rhs = rhs0)
      else tree
    } else tree
  }

  private def desugarIdent(i: Ident)(implicit ctx: Context): Option[Select] = {
    i.tpe match {
      case TermRef(prefix: TermRef, name) =>
        Some(ref(prefix).select(i.symbol))
      case TermRef(prefix: ThisType, name) =>
        Some(This(prefix.cls).select(i.symbol))
      case _ => None
    }
  }

  private def dropCasts(t: Tree)(implicit ctx: Context): Tree = t match {
    // case TypeApply(aio@Select(rec, nm), _) if aio.symbol == defn.Any_asInstanceOf => dropCasts(rec)
    case Typed(t, tpe) => t
    case _ => t
  }

  private def readingOnlyVals(t: Tree)(implicit ctx: Context): Boolean = dropCasts(t) match {
    case Typed(exp, _) => readingOnlyVals(exp)
    case TypeApply(fun @ Select(rec, _), List(tp)) =>
      if ((fun.symbol eq defn.Any_asInstanceOf) && rec.tpe.derivesFrom(tp.tpe.classSymbol))
        readingOnlyVals(rec)
      else false
    case Apply(Select(rec, _), Nil) =>
      def isGetterOfAImmutableField = t.symbol.isGetter && !t.symbol.is(Mutable)
      def isCaseClassWithVar        = t.symbol.info.decls.exists(_.is(Mutable))
      def isAccessingProductField   = t.symbol.exists                               &&
                                      t.symbol.owner.derivesFrom(defn.ProductClass) &&
                                      t.symbol.owner.is(CaseClass)                  &&
                                      t.symbol.name.isSelectorName                  &&
                                      !isCaseClassWithVar // Conservative Covers case class A(var x: Int)
      def isImmutableCaseAccessor   = t.symbol.is(CaseAccessor) && !t.symbol.is(Mutable)
      if (isGetterOfAImmutableField || isAccessingProductField || isImmutableCaseAccessor)
        readingOnlyVals(rec)
      else false
    case Select(rec, _) if t.symbol.is(Method) =>
      if (t.symbol.isGetter && !t.symbol.is(Mutable)) readingOnlyVals(rec) // getter of a immutable field
      else if (t.symbol.owner.derivesFrom(defn.ProductClass) && t.symbol.owner.is(CaseClass) && t.symbol.name.isSelectorName) {
        def isImmutableField = {
          val fieldId = t.symbol.name.toString.drop(1).toInt - 1
          !t.symbol.owner.caseAccessors(ctx)(fieldId).is(Mutable)
        }
        if (isImmutableField) readingOnlyVals(rec) // accessing a field of a product
        else false
      } else if (t.symbol.is(CaseAccessor) && !t.symbol.is(Mutable))
        readingOnlyVals(rec)
      else false
    case Select(qual, _) if !t.symbol.is(Mutable) =>
      readingOnlyVals(qual)
    case t: Ident if !t.symbol.is(Mutable) && !t.symbol.is(Method) && !t.symbol.info.dealias.isInstanceOf[ExprType] =>
      desugarIdent(t) match {
        case Some(t) => readingOnlyVals(t)
        case None => true
      }
    case t: This => true
    // null => false, or the following fails devalify:
    // trait I {
    //   def foo: Any = null
    // }
    // object Main {
    //   def main = {
    //     val s: I = null
    //     s.foo
    //   }
    // }
    case Literal(Constant(null)) => false
    case t: Literal => true
    case _ => false
  }

  /** Inline case class specific methods using desugarings assumptions.
   *
   * -
   * -
   *
   * Dotty only:
   * -
   *
   * Scala2 only:
   * -
   */
  val inlineCaseIntrinsics: Optimization = { implicit ctx: Context =>
    // Apply fun may be a side-effectful function. E.g. a block, see tests/run/t4859.scala
    // we need to maintain expressions that were in this block
    def evalReciever(a: Apply, res: Tree) = {
      def receiver(t: Tree): Tree = t match {
        case TypeApply(fun, targs) if fun.symbol eq t.symbol => receiver(fun)
        case Apply(fn, args) if fn.symbol == t.symbol => receiver(fn)
        case Select(qual, _) => qual
        case x => x
      }
      val recv = receiver(a)
      if (recv.isEmpty || tpd.isPureRef(recv))
        res
      else
        Block(recv :: Nil, res)
    }

    val transformer: Transformer = () => localCtx => {
      // For synthetic applies on case classes (both dotty/scalac)
      // - CC.apply(args) → new CC(args)
      case a: Apply if !a.tpe.isInstanceOf[MethodicType]           &&
                       a.symbol.is(Synthetic)                      &&
                       a.symbol.owner.is(Module)                   &&
                       (a.symbol.name == nme.apply)                &&
                       a.symbol.owner.companionClass.is(CaseClass) &&
                       !a.tpe.derivesFrom(defn.EnumClass)          &&
                       (isPureExpr(a.fun) || a.fun.symbol.is(Synthetic)) =>

        def unrollArgs(t: Tree, l: List[List[Tree]]): List[List[Tree]] = t match {
          case Apply(t, args) => unrollArgs(t, args :: l)
          case _ => l
        }
        val argss = unrollArgs(a.fun, a.args :: Nil)
        def rollInArgs(l: List[List[Tree]], fun: Tree): Tree = l match {
          case head :: tail => rollInArgs(tail, fun.appliedToArgs(head))
          case _ => fun
        }
        val constructor = a.symbol.owner.companionClass.primaryConstructor.asTerm
        rollInArgs(argss.tail, New(a.tpe.widenDealias, constructor, argss.head))

      // For synthetic dotty unapplies on case classes:
      // - CC.unapply(arg): CC → arg
      // - CC.unapply(arg): Boolean → true, dotty only
      // - CC.unapply(arg): Option[CC] → new Some(new scala.TupleN(arg._1, ..., arg._N))
      case a: Apply if a.symbol.is(Synthetic)                       &&
                       a.symbol.owner.is(Module)                    &&
                       (a.symbol.name == nme.unapply)               &&
                       a.symbol.owner.companionClass.is(CaseClass)  &&
                       !a.tpe.derivesFrom(defn.EnumClass)           &&
                       (isPureExpr(a.fun) || a.fun.symbol.is(Synthetic)) =>

        val args = a.args.head
        val isDottyUnapply = !a.symbol.owner.is(Scala2x)
        val isScalaOptionUnapply =
          a.tpe.derivesFrom(defn.OptionClass) &&
          a.args.head.tpe.derivesFrom(a.symbol.owner.companionClass)

        if (isDottyUnapply) { // dotty only
          if (a.tpe.derivesFrom(defn.BooleanClass))
            // CC.unapply(arg): Boolean → true
            evalReciever(a, Literal(Constant(true)))
          else
            // CC.unapply(arg): CC → arg
            evalReciever(a, a.args.head)
        }
        else if (isScalaOptionUnapply) {
          // CC.unapply(arg): Option[CC] → new Some(new scala.TupleN(arg._1, ..., arg._N))
          // The output is defined as a Tree => Tree to go thought tpd.evalOnce.
          def some(e: Tree) = {
            val accessors = e.tpe.widenDealias.classSymbol.caseAccessors.filter(_.is(Method))
            val fields    = accessors.map(x => e.select(x).ensureApplied)
            val tplType   = a.tpe.baseArgTypes(defn.OptionClass).head
            val someTpe   = a.tpe.translateParameterized(defn.OptionClass, defn.SomeClass)

            if (fields.tail.nonEmpty)
              New(someTpe, New(tplType, fields) :: Nil)
            else // scalac does not have Tuple1
              New(someTpe, fields.head :: Nil)
          }
          val none = ref(defn.NoneModuleRef)
          def isNull(e: Tree) = e.select(defn.Object_eq).appliedTo(Literal(Constant(null)))
          def fi(e: Tree) = If(isNull(e), none, some(e))
          evalReciever(a, evalOnce(a.args.head)(fi))
        }
        else a

      // Seq.unapplySeq(arg) → new Some(arg)
      // Where Seq is any companion of type <: SeqFactoryClass
      case a: Apply if (a.symbol.name == nme.unapplySeq)           &&
                       a.symbol.owner.derivesFrom(SeqFactoryClass) &&
                       a.symbol.extendedOverriddenSymbols.isEmpty  &&
                       (isPureExpr(a.fun) || a.fun.symbol.is(Synthetic)) =>

        def reciever(t: Tree): Type = t match {
          case t: Apply     => reciever(t.fun)
          case t: TypeApply => reciever(t.fun)
          case t: Ident     => desugarIdent(t) match {
            case Some(t) => reciever(t)
            case _ => NoType
          }
          case t: Select => t.qualifier.tpe.widenDealias
        }

        val recv = reciever(a)
        if (recv.typeSymbol.is(Module)) {
          val someTpe  = a.tpe.translateParameterized(defn.OptionClass, defn.SomeClass)
          evalReciever(a, New(someTpe, a.args.head :: Nil))
        }
        else a
      case t => t
    }
    // To run this optimisation after erasure one would need to specialize it
    // for constructor with outer pointer and values classes. There is probably
    // no need to run this more than once.
    ("inlineCaseIntrinsics", BeforeErasure, NoVisitor, transformer)
  }

  /** Various constant folding.
   *
   *  - Starts/ends with the constant folding implemented in typer (ConstFold).
   *
   *  - (if) specific optimization that propagate booleans, negation, and factor
   *    out (nested) if with equivalent branches wrt to isSimilar (using &&,||).
   *
   *  - Constant propagation over pattern matching.
   */
  val constantFold: Optimization = { implicit ctx: Context =>
    def preEval(t: Tree) = {
      if (t.isInstanceOf[Literal] || t.isInstanceOf[CaseDef] || !isPureExpr(t)) t
      else {
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
            (t1.symbol == t2.symbol) &&
            (t1.args zip t2.args).forall(x => isSimilar(x._1, x._2)) &&
            isSimilar(t1.fun, t2.fun)
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
        case t2: Select =>
          (t1.symbol eq t2.symbol) &&
          isSimilar(t1.qualifier, t2.qualifier)
        case t2: Ident => desugarIdent(t2) match {
          case Some(t2) => isSimilar(t1, t2)
          case None => false
        }
        case _ => false
      }
      case t1: Literal => t2 match {
        case t2: Literal =>
          t1.const.tag   == t2.const.tag &&
          t1.const.value == t2.const.value
        case _ => false
      }
      case _ => false
    }

    def isBool(tpe: Type): Boolean = tpe.derivesFrom(defn.BooleanClass)
    def isConst(tpe: Type): Boolean      = tpe.isInstanceOf[ConstantType]
    def asConst(tpe: Type): ConstantType = tpe.asInstanceOf[ConstantType]

    val transformer: Transformer = () => localCtx => { x => preEval(x) match {
      // TODO: include handling of isInstanceOf similar to one in IsInstanceOfEvaluator
      // TODO: include methods such as Int.int2double(see ./tests/pos/harmonize.scala)
      case If(cond1, thenp, elsep) if isSimilar(thenp, elsep) =>
        Block(cond1 :: Nil, thenp)

      case If(cond1, If(cond2, thenp2, elsep2), elsep1) if isSimilar(elsep1, elsep2) =>
        If(cond1.select(defn.Boolean_&&).appliedTo(cond2), thenp2, elsep1)

      case If(cond1, If(cond2, thenp2, elsep2), elsep1) if isSimilar(elsep1, thenp2) =>
        If(cond1.select(defn.Boolean_!).ensureApplied.select(defn.Boolean_||).appliedTo(cond2), elsep1, elsep2)

      case If(cond1, thenp1, If(cond2, thenp2, elsep2)) if isSimilar(thenp1, thenp2) =>
        If(cond1.select(defn.Boolean_||).appliedTo(cond2), thenp1, elsep2)

      case If(cond1, thenp1, If(cond2, thenp2, elsep2)) if isSimilar(thenp1, elsep2) =>
        If(cond1.select(defn.Boolean_||).appliedTo(cond2.select(defn.Boolean_!).ensureApplied), thenp1, thenp2)

      case If(t: Literal, thenp, elsep) =>
        if (t.const.booleanValue) thenp
        else elsep

      case ift @ If(cond, thenp: Literal, elsep: Literal)
        if isBool(ift.tpe) && thenp.const.booleanValue && !elsep.const.booleanValue =>
          cond

      //  the lower two are disabled, as it may make the isSimilar rule not apply for a nested structure of iffs.
      //  see the example below:
      //         (b1, b2) match {
      //           case (true, true) => true
      //           case (false, false) => true
      //           case _ => false
      //         }
      // case ift @ If(cond, thenp: Literal, elsep)
      //   if isBool(ift.tpe) && thenp.const.booleanValue =>
      //     if (thenp.const.booleanValue)
      //       cond.select(defn.Boolean_||).appliedTo(elsep)
      //     else // thenp is false, this tree is bigger then the original
      //       cond.select(defn.Boolean_!).ensureApplied.select(defn.Boolean_&&).appliedTo(elsep)
      // case ift @ If(cond, thenp, elsep :Literal) if
      //    isBool(ift.tpe) && !elsep.const.booleanValue =>
      //       cond.select(defn.Boolean_&&).appliedTo(elsep)
      //   the other case ins't handled intentionally. See previous case for explanation

      case If(t @ Select(recv, _), thenp, elsep) if t.symbol eq defn.Boolean_! =>
        If(recv, elsep, thenp)

      case If(t @ Apply(Select(recv, _), Nil), thenp, elsep) if t.symbol eq defn.Boolean_! =>
        If(recv, elsep, thenp)

      // TODO: similar trick for comparisons.
      // TODO: handle comparison with min\max values
      case Apply(meth1 @ Select(Apply(meth2 @ Select(rec, _), Nil), _), Nil)
        if meth1.symbol == defn.Boolean_! && meth2.symbol == defn.Boolean_! =>
          rec

      case meth1 @ Select(meth2 @ Select(rec, _), _)
        if meth1.symbol == defn.Boolean_! && meth2.symbol == defn.Boolean_! && !ctx.erasedTypes =>
          rec

      case t @ Apply(Select(lhs, _), List(rhs)) =>
        val sym = t.symbol
        (lhs, rhs) match {
          case (lhs, Literal(_)) if !lhs.isInstanceOf[Literal] && symmetricOperations.contains(sym) =>
            rhs.select(sym).appliedTo(lhs)
          case (l, _) if (sym == defn.Boolean_&&) && isConst(l.tpe) =>
            val const = asConst(l.tpe).value.booleanValue
            if (const) Block(lhs :: Nil, rhs)
            else l

          case (l, x: Literal) if sym == defn.Boolean_== && isBool(l.tpe) && isBool(x.tpe) =>
            if (x.const.booleanValue) l
            else l.select(defn.Boolean_!).ensureApplied

          case (l, x: Literal) if sym == defn.Boolean_!= && isBool(l.tpe) && isBool(x.tpe) =>
            if (!x.const.booleanValue) l
            else l.select(defn.Boolean_!).ensureApplied

          case (x: Literal, l) if sym == defn.Boolean_== && isBool(l.tpe) && isBool(x.tpe) =>
            if (x.const.booleanValue) l
            else l.select(defn.Boolean_!).ensureApplied

          case (x: Literal, l) if sym == defn.Boolean_!= && isBool(l.tpe) && isBool(x.tpe) =>
            if (!x.const.booleanValue) l
            else l.select(defn.Boolean_!).ensureApplied

          case (l: Literal, _) if (sym == defn.Boolean_||) && isConst(l.tpe)   =>
            val const = asConst(l.tpe).value.booleanValue
            if (l.const.booleanValue) l
            else Block(lhs :: Nil, rhs)

          // case (Literal(Constant(1)), _)    if sym == defn.Int_*  => rhs
          // case (Literal(Constant(0)), _)    if sym == defn.Int_+  => rhs
          // case (Literal(Constant(1L)), _)   if sym == defn.Long_* => rhs
          // case (Literal(Constant(0L)), _)   if sym == defn.Long_+ => rhs
          // // TODO: same for float, double, short
          // // TODO: empty string concat
          // // TODO: disctribute & reorder constants
          // // TODO: merge subsequent casts
          // case (_, Literal(Constant(1)))    if sym == defn.Int_/  => lhs
          // case (_, Literal(Constant(1L)))   if sym == defn.Long_/ => lhs
          // case (_, Literal(Constant(0)))    if sym == defn.Int_/  =>
          //   Block(List(lhs),
          //     ref(defn.throwMethod).appliedTo(New(defn.ArithmeticExceptionClass.typeRef, defn.ArithmeticExceptionClass_stringConstructor, Literal(Constant("/ by zero")) :: Nil)))
          // case (_, Literal(Constant(0L)))  if sym == defn.Long_/ =>
          //   Block(List(lhs),
          //     ref(defn.throwMethod).appliedTo(New(defn.ArithmeticExceptionClass.typeRef, defn.ArithmeticExceptionClass_stringConstructor, Literal(Constant("/ by zero")) :: Nil)))

          case _ => t
        }

      // This case can only be triggered when running Simplify before pattern matching:
      // case t: Match
      //   if t.selector.tpe.isInstanceOf[ConstantType] &&
      //      t.cases.forall { x =>
      //        x.pat.tpe.isInstanceOf[ConstantType] || (isWildcardArg(x.pat) && x.guard.isEmpty)
      //      } =>
      //   val selectorValue = t.selector.tpe.asInstanceOf[ConstantType].value
      //   val better = t.cases.find(x => isWildcardArg(x.pat) || (x.pat.tpe.asInstanceOf[ConstantType].value eq selectorValue))
      //   if (better.nonEmpty) better.get.body
      //   else t

      case t: Literal => t
      case t: CaseDef => t
      case t if !isPureExpr(t) => t
      case t =>
        val s = ConstFold.apply(t)
        if ((s ne null) && s.tpe.isInstanceOf[ConstantType]) {
          val constant = s.tpe.asInstanceOf[ConstantType].value
          Literal(constant)
        } else t
    }}
    ("constantFold", BeforeAndAfterErasure, NoVisitor, transformer)
  }

  /** Inline case classes as vals, this essentially (local) implements multi
   *  parameter value classes. The main motivation is to get ride of all the
   *  intermediate tuples coming from pattern matching expressions.
   */
  val inlineLocalObjects: Optimization = { implicit ctx: Context =>
    // In the end only calls constructor. Reason for unconditional inlining
    val hasPerfectRHS = mutable.HashMap[Symbol, Boolean]()
    // If all values have perfect RHS than key has perfect RHS
    val checkGood = mutable.HashMap[Symbol, Set[Symbol]]()
    val forwarderWritesTo = mutable.HashMap[Symbol, Symbol]()
    val gettersCalled = mutable.HashSet[Symbol]()
    def followTailPerfect(t: Tree, symbol: Symbol): Unit = {
      t match {
        case Block(_, expr) => followTailPerfect(expr, symbol)
        case If(_, thenp, elsep) => followTailPerfect(thenp, symbol); followTailPerfect(elsep, symbol);
        case Apply(fun, _) if fun.symbol.isConstructor && t.tpe.widenDealias == symbol.info.widenDealias.finalResultType.widenDealias =>
          hasPerfectRHS(symbol) = true
        case Apply(fun, _) if fun.symbol.is(Label) && (fun.symbol ne symbol) =>
          checkGood.put(symbol, checkGood.getOrElse(symbol, Set.empty) + fun.symbol)
          // assert(forwarderWritesTo.getOrElse(t.symbol, symbol) == symbol)
          forwarderWritesTo(t.symbol) = symbol
        case t: Ident if !t.symbol.owner.isClass && (t.symbol ne symbol) =>
          checkGood.put(symbol, checkGood.getOrElse(symbol, Set.empty) + t.symbol)
        case _ =>
      }
    }
    val visitor: Visitor = {
      case vdef: ValDef if (vdef.symbol.info.classSymbol is CaseClass) &&
                           !vdef.symbol.is(Lazy)                       &&
                           !vdef.symbol.info.classSymbol.caseAccessors.exists(x => x.is(Mutable)) =>
        followTailPerfect(vdef.rhs, vdef.symbol)
      case Assign(lhs, rhs) if !lhs.symbol.owner.isClass =>
        checkGood.put(lhs.symbol, checkGood.getOrElse(lhs.symbol, Set.empty) + rhs.symbol)
      case t @ Select(qual, _) if (t.symbol.isGetter && !t.symbol.is(Mutable)) ||
        (t.symbol.maybeOwner.derivesFrom(defn.ProductClass) && t.symbol.maybeOwner.is(CaseClass) && t.symbol.name.isSelectorName) ||
        (t.symbol.is(CaseAccessor) && !t.symbol.is(Mutable)) =>
        gettersCalled(qual.symbol) = true
      case t: DefDef if t.symbol.is(Label) =>
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
        hasPerfectRHS.iterator.map(x => x._1).filter(x => !x.is(Method) && !x.is(Label) && gettersCalled.contains(x.symbol) && (x.symbol.info.classSymbol is CaseClass))
          .map { refVal =>
            simplify.println(s"replacing ${refVal.symbol.fullName} with stack-allocated fields")
            var accessors = refVal.info.classSymbol.caseAccessors.filter(_.isGetter) // TODO: drop mutable ones
            if (accessors.isEmpty) accessors = refVal.info.classSymbol.caseAccessors
            val productAccessors = (1 to accessors.length).map(i => refVal.info.member(nme.productAccessorName(i)).symbol) // TODO: disambiguate
            val newLocals = accessors.map(x =>
              // TODO: it would be nice to have an additional optimization that
              // TODO: is capable of turning those mutable ones into immutable in common cases
              ctx.newSymbol(ctx.owner.enclosingMethod, (refVal.name + "$" + x.name).toTermName, Synthetic | Mutable, x.asSeenFrom(refVal.info).info.finalResultType.widenDealias)
            )
            val fieldMapping = accessors zip newLocals
            val productMappings = productAccessors zip newLocals
            (refVal, (fieldMapping ++ productMappings).toMap)
          }.toMap
      val toSplit: mutable.Set[Symbol] = mutable.Set.empty ++ newMappings.keySet

      def splitWrites(t: Tree, target: Symbol): Tree = {
        t match {
          case tree@ Block(stats, expr) => tpd.cpy.Block(tree)(stats, splitWrites(expr, target))
          case tree@ If(_, thenp, elsep) => tpd.cpy.If(tree)(thenp = splitWrites(thenp, target), elsep =  splitWrites(elsep, target))
          case Apply(sel , args) if sel.symbol.isConstructor && t.tpe.widenDealias == target.info.widenDealias.finalResultType.widenDealias =>
            val fieldsByAccessors = newMappings(target)
            var accessors = target.info.classSymbol.caseAccessors.filter(_.isGetter) // TODO: when is this filter needed?
            if (accessors.isEmpty) accessors = target.info.classSymbol.caseAccessors
            val assigns = (accessors zip args) map (x => ref(fieldsByAccessors(x._1)).becomes(x._2))
            val recreate = sel.appliedToArgs(accessors.map(x => ref(fieldsByAccessors(x))))
            Block(assigns, recreate)
          case Apply(fun, _) if fun.symbol.is(Label) =>
            t // Do nothing. It will do on its own.
          case t: Ident if !t.symbol.owner.isClass && newMappings.contains(t.symbol) && t.symbol.info.classSymbol == target.info.classSymbol =>
            val fieldsByAccessorslhs = newMappings(target)
            val fieldsByAccessorsrhs = newMappings(t.symbol)
            val accessors = target.info.classSymbol.caseAccessors.filter(_.isGetter)
            val assigns = accessors map (x => ref(fieldsByAccessorslhs(x)).becomes(ref(fieldsByAccessorsrhs(x))))
            Block(assigns, t)
          // If `t` is itself split, push writes.
          case _ =>
            evalOnce(t){ev =>
              if (ev.tpe.derivesFrom(defn.NothingClass)) ev
              else {
                val fieldsByAccessors = newMappings(target)
                val accessors = target.info.classSymbol.caseAccessors.filter(_.isGetter)
                val assigns = accessors map (x => ref(fieldsByAccessors(x)).becomes(ev.select(x)))
                Block(assigns, ev)
              }
            } // Need to eval-once and update fields.

        }
      }

      def followCases(t: Symbol, limit: Int = 0): Symbol = if (t.symbol.is(Label)) {
        // TODO: this can create cycles, see ./tests/pos/rbtree.scala
        if (limit > 100 && limit > forwarderWritesTo.size + 1) NoSymbol
          // There may be cycles in labels, that never in the end write to a valdef(the value is always on stack)
          // there's not much we can do here, except finding such cases and bailing out
          // there may not be a cycle bigger that hashmapSize > 1
        else followCases(forwarderWritesTo.getOrElse(t.symbol, NoSymbol), limit + 1)
      } else t

      hasPerfectRHS.clear()
      // checkGood.clear()
      gettersCalled.clear()

      val res: Context => Tree => Tree = {localCtx => (t: Tree) => t match {
        case ddef: DefDef if ddef.symbol.is(Label) =>
          newMappings.get(followCases(ddef.symbol)) match {
            case Some(mappings) =>
              tpd.cpy.DefDef(ddef)(rhs = splitWrites(ddef.rhs, followCases(ddef.symbol)))
            case _ => ddef
          }
        case a: ValDef if toSplit.contains(a.symbol) =>
          toSplit -= a.symbol
          // Break ValDef apart into fields + boxed value
          val newFields = newMappings(a.symbol).values.toSet
          Thicket(
            newFields.map(x => ValDef(x.asTerm, defaultValue(x.symbol.info.widenDealias))).toList :::
              List(tpd.cpy.ValDef(a)(rhs = splitWrites(a.rhs, a.symbol))))
        case ass: Assign =>
          newMappings.get(ass.lhs.symbol) match {
            case None =>   ass
            case Some(mapping) =>
              val updates = mapping.filter(x => x._1.is(CaseAccessor)).map(x => ref(x._2).becomes(ref(ass.lhs.symbol).select(x._1))).toList
              Thicket(ass :: updates)
          }
        case sel @ Select(rec, _) if (t.symbol.isGetter && !t.symbol.is(Mutable)) ||
          (t.symbol.maybeOwner.derivesFrom(defn.ProductClass) && t.symbol.owner.is(CaseClass) && t.symbol.name.isSelectorName) ||
          (t.symbol.is(CaseAccessor) && !t.symbol.is(Mutable)) =>
          newMappings.getOrElse(rec.symbol, Map.empty).get(sel.symbol) match {
            case None => t
            case Some(newSym) => ref(newSym)
          }
        case t => t
      }}

    res
    }
    ("inlineLocalObjects", BeforeAndAfterErasure, visitor, transformer)
  }

  private def collectTypeTests(t: Tree)(implicit ctx: Context): List[(Symbol, Type)] = {
    def recur(t: Tree): List[(Symbol, Type)] =
      t match {
        case Apply(x, _) if (x.symbol == defn.Boolean_! || x.symbol == defn.Boolean_||) => List.empty
        case Apply(fun @ Select(x, _), y) if (fun.symbol == defn.Boolean_&&) => recur(x) ++ recur(y.head)
        case TypeApply(fun @ Select(x, _), List(tp)) if fun.symbol eq defn.Any_isInstanceOf =>
          if (x.symbol.exists && !x.symbol.owner.isClass && !x.symbol.is(Method|Mutable))
            (x.symbol, tp.tpe) :: Nil
          else Nil
        case _ => List.empty
      }
    recur(t)
  }

  private def collectNullTests(t: Tree)(implicit ctx: Context): List[Symbol] = {
    def recur(t: Tree): List[Symbol] =
      t match {
        case Apply(x, _) if (x.symbol == defn.Boolean_! || x.symbol == defn.Boolean_||) => List.empty
        case Apply(fun @ Select(x, _), y) if (fun.symbol == defn.Boolean_&&) => recur(x) ++ recur(y.head)
        case Apply(fun @ Select(x, _), List(tp)) if fun.symbol eq defn.Object_ne =>
          if (x.symbol.exists && !x.symbol.owner.isClass && !x.symbol.is(Method|Mutable))
            x.symbol :: Nil
          else Nil
        case _ => List.empty
      }
    recur(t)
  }

  /** Eliminated casts and equality tests whose results can be locally
   *  determined at compile time:
   *
   *  - a.asInstanceOf[T] → a when we know that a: T
   *  - Simplify (a == null) and (a != null) when the result is statically known
   */
  val dropGoodCasts: Optimization = { implicit ctx: Context =>
    val transformer: Transformer = () => localCtx => {
      case t @ If(cond, thenp, elsep) =>
        val newTypeTested = collectTypeTests(cond)
        val nullTested = collectNullTests(cond).toSet
        val testedMap = newTypeTested.foldRight[Map[Symbol, List[Type]]](Map.empty) { case (x, y) =>
          y + ((x._1, x._2 :: y.getOrElse(x._1, Nil)))
        }
        val dropGoodCastsInStats = new TreeMap() {
          override def transform(tree: Tree)(implicit ctx: Context): Tree = {
            def applyCondition(fun: Select, tree: Tree, const: Constant): Boolean =
              const.tag == Constants.NullTag &&
              (fun.symbol == defn.Object_eq || fun.symbol == defn.Object_ne) &&
              (nullTested.contains(tree.symbol))

            def applyBody(fun: Select): Tree =
              if (fun.symbol == defn.Object_eq) Literal(Constant(false))
              else Literal(Constant(true))

            super.transform(tree) match {
              case t: Block =>
                val nstats = t.stats.filterConserve({
                  case TypeApply(fun @ Select(rec, _), List(tp))
                    if fun.symbol == defn.Any_asInstanceOf =>
                      !testedMap.getOrElse(rec.symbol, Nil).exists(x => x <:< tp.tpe)
                  case _ => true
                })
                if (nstats eq t.stats) t
                else Block(nstats, t.expr)
              case Apply(fun @ Select(lhs, _), List(Literal(const))) if applyCondition(fun, lhs, const) =>
                applyBody(fun)
              case Apply(fun @ Select(Literal(const), _), List(rhs)) if applyCondition(fun, rhs, const) =>
                applyBody(fun)
              case t => t
            }
          }
        }
        val nthenp = dropGoodCastsInStats.transform(thenp)

        tpd.cpy.If(t)(thenp = nthenp, elsep = elsep)
      case t => t
    }
    ("dropGoodCasts", BeforeAndAfterErasure, NoVisitor, transformer)
  }

  /** Eliminated null checks based on the following observations:
   *
   *  - (this)  cannot be null
   *  - (new C) cannot be null
   *  - literal is either null itself or non null
   *  - fallsback to `tpe.isNotNull`, which will eventually be true for non nullable types.
   *  - in (a.call; a == null), the first call throws a NPE if a is null; the test can be removed.
   */
  val removeUnnecessaryNullChecks: Optimization = { implicit ctx: Context =>
    val initializedVals = mutable.HashSet[Symbol]()
    val checkGood = mutable.HashMap[Symbol, Set[Symbol]]()
    def isGood(t: Symbol) = {
      t.exists && initializedVals.contains(t) && {
        var changed = true
        var set = Set(t)
        while (changed) {
          val oldSet = set
          set = set ++ set.flatMap(x => checkGood.getOrElse(x, Nil))
          changed = set != oldSet
        }
        !set.exists(x => !initializedVals.contains(x))
      }
    }
    val visitor: Visitor = {
      case vd: ValDef =>
        val rhs = vd.rhs
        val rhsName = rhs.symbol.name
        if (!vd.symbol.is(Mutable) && !rhs.isEmpty) {
          def checkNonNull(t: Tree, target: Symbol): Boolean = t match {
            case Block(_ , expr) => checkNonNull(expr, target)
            case If(_, thenp, elsep) => checkNonNull(thenp, target) && checkNonNull(elsep, target)
            case t: New => true
            case t: Apply if t.symbol.isPrimaryConstructor => true
            case t: Literal => t.const.value != null
            case t: This => true
            case t: Ident if !t.symbol.owner.isClass =>
              checkGood.put(target, checkGood.getOrElse(target, Set.empty) + t.symbol)
              true
            case t: Apply if !t.symbol.owner.isClass =>
              checkGood.put(target, checkGood.getOrElse(target, Set.empty) + t.symbol)
              true
            case t: Typed =>
              checkNonNull(t.expr, target)
            case _ => t.tpe.isNotNull
          }
          if (checkNonNull(vd.rhs, vd.symbol))
            initializedVals += vd.symbol
        }
      case t: Tree =>
    }

    def isNullLiteral(tree: Tree) = tree match {
      case literal: Literal =>
        literal.const.tag == Constants.NullTag
      case _ => false
    }
    val transformer: Transformer = () => localCtx0 => {
      implicit val ctx: Context = localCtx0
      val transformation: Tree => Tree = {
        case check@Apply(Select(lhs, _), List(rhs)) =>
          val sym = check.symbol
          if ( ((sym == defn.Object_eq) || (sym == defn.Object_ne)) &&
            ((isNullLiteral(lhs) && isGood(rhs.symbol)) || (isNullLiteral(rhs) && isGood(lhs.symbol)))) {
            if (sym == defn.Object_eq) Block(List(lhs, rhs), Literal(Constant(false)))
            else if(sym == defn.Object_ne) Block(List(lhs, rhs), Literal(Constant(true)))
            else check
          } else check
        case t => t
      }
      transformation
    }
    ("removeUnnecessaryNullChecks", BeforeErasure, visitor,
      transformer)
  }

  /** Every pure statement preceding a ??? can be removed.
   *
   *  This optimization makes it rather tricky meaningful examples since the
   *  compiler will often be able to reduce them to a single main with ???...
   */
  val bubbleUpNothing: Optimization = { implicit ctx: Context =>
    object Notathing {
      def unapply(t: Tree): Option[Tree] = Option(lookup(t))
      def lookup(t: Tree): Tree = t match {
        case x if x.tpe.derivesFrom(defn.NothingClass) => t
        case Typed(x, _) => lookup(x)
        case Block(_, x) => lookup(x)
        case _ => null
      }
    }
    def notathing(t: Tree): Boolean = t match {
      case Notathing(_) => true
      case _ => false
    }
    val transformer: Transformer = () => localCtx => {
      case t @ Apply(Select(Notathing(qual), _), args) =>
        Typed(qual, TypeTree(t.tpe))
      // This case leads to complications with multiple argument lists,
      // how to do you rewrites tree.witType(???)(ctx).withType(???)(ctx)
      // using Ycheckable steps?

      // Solution: only transform when having a complete application,
      // steal code from tailRec

      // case t @ Apply(Select(qual, _), args) if args.exists(notathing) =>
      //   val (keep, noth :: other) = args.span(x => !notathing(x))
      //   Block(qual :: keep, Typed(noth, TypeTree(t.tpe)))
      case Assign(_, rhs) if notathing(rhs) =>
        rhs
      case t @ If(Notathing(cond), _, _) =>
        Typed(cond, TypeTree(t.tpe))
      case b: Block if b.stats.exists(x => !x.isDef && notathing(x)) =>
        val (keep, noth :: other) = b.stats.span(x => x.isDef || !notathing(x))
        val keepDefs = other.filter(x => x.isDef)
        val body = keep ::: keepDefs
        Typed(Block(body, noth), TypeTree(b.tpe))
      case t => t
    }
    ("bubbleUpNothing", BeforeErasure, NoVisitor, transformer)
  }

  private def keepOnlySideEffects(t: Tree)(implicit ctx: Context): Tree = {
    t match {
      case l: Literal =>
        EmptyTree
      case t: This =>
        EmptyTree
      case Typed(exp, tpe) =>
        keepOnlySideEffects(exp)
      case t @ If(cond, thenp, elsep) =>
        val nthenp = keepOnlySideEffects(thenp)
        val nelsep = keepOnlySideEffects(elsep)
        if (thenp.isEmpty && elsep.isEmpty) keepOnlySideEffects(cond)
        else tpd.cpy.If(t)(
          thenp = nthenp.orElse(if (thenp.isInstanceOf[Literal]) thenp else unitLiteral),
          elsep = nelsep.orElse(if (elsep.isInstanceOf[Literal]) elsep else unitLiteral))
      case Select(rec, _) if
        (t.symbol.isGetter && !t.symbol.is(Mutable | Lazy)) ||
        (t.symbol.owner.derivesFrom(defn.ProductClass) && t.symbol.owner.is(CaseClass) && t.symbol.name.isSelectorName) ||
        (t.symbol.is(CaseAccessor) && !t.symbol.is(Mutable)) =>
          keepOnlySideEffects(rec) // Accessing a field of a product
      case s @ Select(qual, name) if
          // !name.eq(nme.TYPE_) && // Keep the .TYPE added by ClassOf, would be needed for AfterErasure
          !t.symbol.is(Mutable | Lazy) && !t.symbol.is(Method) =>
        keepOnlySideEffects(qual)
      case Block(List(t: DefDef), s: Closure) =>
        EmptyTree
      case bl @ Block(stats, expr) =>
        val stats1 = stats.mapConserve(keepOnlySideEffects)
        val stats2 = if (stats1 ne stats) stats1.filter(x=>x ne EmptyTree) else stats1
        val expr2: Tree = expr match {
          case t: Literal if t.tpe.derivesFrom(defn.UnitClass) => expr
          case _ => keepOnlySideEffects(expr).orElse(unitLiteral)
        }
        tpd.cpy.Block(bl)(stats2, expr2)
      case t: Ident if !t.symbol.is(Method | Lazy) && !t.symbol.info.isInstanceOf[ExprType] || Analysis.effectsDontEscape(t) =>
        desugarIdent(t) match {
          case Some(t) if !(t.qualifier.symbol.is(Flags.JavaDefined) && t.qualifier.symbol.is(Flags.Package)) => t
          case _ => EmptyTree
        }
      case app: Apply if app.fun.symbol.is(Label) && !app.tpe.finalResultType.derivesFrom(defn.UnitClass) =>
        // This is "the scary hack". It changes the return type to Unit, then
        // invalidates the denotation cache. Because this optimization only
        // operates locally, this should be fine.
        val denot = app.fun.symbol.denot
        if (!denot.info.finalResultType.derivesFrom(defn.UnitClass)) {
          val newLabelType = app.symbol.info match {
            case mt: MethodType =>
              mt.derivedLambdaType(mt.paramNames, mt.paramInfos, defn.UnitType)
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
        Block(prefix, unitLiteral)
      case t @ TypeApply(Select(rec, _), List(testType)) if t.symbol.eq(defn.Any_asInstanceOf) && testType.tpe.widenDealias.typeSymbol.exists =>
        val receiverType = TypeErasure.erasure(rec.tpe)
        val erazedTestedType = TypeErasure.erasure(testType.tpe)
        if (receiverType.derivesFrom(erazedTestedType.typeSymbol))
          EmptyTree
        else t
      case _ => t
    }
  }

  /** Removes side effect free statements in blocks. */
  val dropNoEffects: Optimization = { implicit ctx: Context =>
    val transformer: Transformer = () => localCtx => {
      case Block(Nil, expr) => expr
      case a: Block  =>
        val newStats0 = a.stats.mapConserve(keepOnlySideEffects)
        val newStats1 = if (newStats0 eq a.stats) newStats0 else newStats0.flatMap {
          case x: Block => x.stats ::: List(x.expr)
          case EmptyTree => Nil
          case t => t :: Nil
        }
        val (newStats2, newExpr) = a.expr match {
          case Block(stats2, expr) => (newStats1 ++ stats2, expr)
          case _ => (newStats1, a.expr)
        }
        if (newStats2.nonEmpty)
          tpd.cpy.Block(a)(stats = newStats2, newExpr)
        else newExpr
      case a: DefDef =>
        if (a.symbol.info.finalResultType.derivesFrom(defn.UnitClass) &&
            !a.rhs.tpe.derivesFrom(defn.UnitClass)                    &&
            !a.rhs.tpe.derivesFrom(defn.NothingClass)) {
          def insertUnit(t: Tree) = {
            if (!t.tpe.derivesFrom(defn.UnitClass)) Block(t :: Nil, unitLiteral)
            else t
          }
          tpd.cpy.DefDef(a)(rhs = insertUnit(keepOnlySideEffects(a.rhs)), tpt = TypeTree(defn.UnitType))
        } else a
      case t => t
    }
    // BoxedUnit messes up this phase after erasure
    ("dropNoEffects", BeforeErasure, NoVisitor, transformer)
  }

  /** Inlines LabelDef which are used exactly once. */
  val inlineLabelsCalledOnce: Optimization = { implicit ctx: Context =>
    val timesUsed = mutable.HashMap[Symbol, Int]()
    val defined = mutable.HashMap[Symbol, DefDef]()

    val visitor: Visitor = {
      case defdef: DefDef if defdef.symbol.is(Label)  =>
        var isRecursive = false
        defdef.rhs.foreachSubTree(x => if (x.symbol == defdef.symbol) isRecursive = true)
        if (!isRecursive) defined.put(defdef.symbol, defdef)
      case t: Apply if t.symbol.is(Label) =>
        val b4 = timesUsed.getOrElseUpdate(t.symbol, 0)
        timesUsed.put(t.symbol, b4 + 1)
      case _ =>
    }

    val transformer: Transformer = () => localCtx => {
      case a: Apply =>
        defined.get(a.symbol) match {
          case None => a
          case Some(defDef) if a.symbol.is(Label) && timesUsed.getOrElse(a.symbol, 0) == 1 && a.symbol.info.paramInfoss == List(Nil) =>
            simplify.println(s"Inlining labeldef ${defDef.name}")
            defDef.rhs.changeOwner(defDef.symbol, localCtx.owner)
          case Some(defDef) if defDef.rhs.isInstanceOf[Literal] =>
            defDef.rhs
          case Some(_) =>
            a
        }
      case a: DefDef if (a.symbol.is(Label) && timesUsed.getOrElse(a.symbol, 0) == 1 && defined.contains(a.symbol)) =>
        simplify.println(s"Dropping labeldef (used once) ${a.name} ${timesUsed.get(a.symbol)}")
        defined.put(a.symbol, a)
        EmptyTree
      case a: DefDef if (a.symbol.is(Label) && timesUsed.getOrElse(a.symbol, 0) == 0 && defined.contains(a.symbol)) =>
        simplify.println(s"Dropping labeldef (never used) ${a.name} ${timesUsed.get(a.symbol)}")
        EmptyTree
      case t => t
    }
    ("inlineLabelsCalledOnce", BeforeErasure, visitor, transformer)
  }

  /** Rewrites pairs of consecutive LabelDef jumps by jumping directly to the target. */
  val jumpjump: Optimization = { implicit ctx: Context =>
    // Optimize label defs that call other label-defs
    val defined = mutable.HashMap[Symbol, Symbol]()

    val visitor: Visitor = {
      case defdef: DefDef if defdef.symbol.is(Label)  =>
        defdef.rhs match {
          case Apply(t, args) if t.symbol.is(Label) &&
            TypeErasure.erasure(defdef.symbol.info.finalResultType).classSymbol ==
            TypeErasure.erasure(t.symbol.info.finalResultType).classSymbol             &&
            args.size == defdef.vparamss.map(_.size).sum                               &&
            args.zip(defdef.vparamss.flatten).forall(x => x._1.symbol eq x._2.symbol)  &&
            !(defdef.symbol eq t.symbol) =>
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
        simplify.println(s"Dropping ${a.symbol.showFullName} as forwarder to ${defined(a.symbol).showFullName}")
        EmptyTree
      case t => t
    }
    ("jumpjump", BeforeAndAfterErasure, visitor, transformer)
  }

  /** Inlines Option methods whose result is known statically. */
  val inlineOptions: Optimization = { implicit ctx: Context =>
    val somes = mutable.HashMap[Symbol, Tree]()
    val nones = mutable.HashSet[Symbol]()

    val visitor: Visitor = {
      case valdef: ValDef if !valdef.symbol.is(Mutable) &&
        valdef.rhs.isInstanceOf[Apply] && valdef.rhs.tpe.derivesFrom(defn.SomeClass) &&
        valdef.rhs.symbol.isPrimaryConstructor =>
        val Apply(_, value) = valdef.rhs
        somes(valdef.symbol) = value.head

      case valdef: ValDef if !valdef.symbol.is(Mutable) &&
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
    ("inlineOptions", BeforeErasure, visitor, transformer)
  }

  /** Rewrite vars with exactly one assignment as vals. */
  val valify: Optimization = { implicit ctx: Context =>
    // Either a duplicate or a read through series of immutable fields.
    val defined: mutable.Map[Symbol, ValDef] = mutable.Map()
    val firstRead: mutable.Map[Symbol, RefTree] = mutable.Map()
    val firstWrite: mutable.Map[Symbol, Assign] = mutable.Map()
    val secondWrite: mutable.Map[Symbol, Assign] = mutable.Map()
    val visitor: Visitor = {
      case t: ValDef if t.symbol.is(Mutable, Lazy) && !t.symbol.is(Method) && !t.symbol.owner.isClass =>
        if (isPureExpr(t.rhs))
          defined(t.symbol) = t

      case t: RefTree if t.symbol.exists && !t.symbol.is(Method) && !t.symbol.owner.isClass =>
        if (!firstWrite.contains(t.symbol)) firstRead(t.symbol) = t

      case t @ Assign(l, expr) if !l.symbol.is(Method) && !l.symbol.owner.isClass =>
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
        case t: Block => // Drop non-side-effecting stats
          val valdefs = t.stats.collect {
            case t: ValDef if defined.contains(t.symbol) => t
          }

          val assigns = t.stats.filter {
            case t @ Assign(lhs, r) =>
              firstWrite.contains(lhs.symbol) && !secondWrite.contains(lhs.symbol)
            case _ => false
          }

          val pairs = valdefs.flatMap(x => assigns.find(y => y.asInstanceOf[Assign].lhs.symbol == x.symbol) match {
            case Some(y: Assign) => List((x, y))
            case _ => Nil
          })

          val valsToDrop = pairs.map(_._1).toSet
          val assignsToReplace: Map[Assign, ValDef] = pairs.map(_.swap).toMap

          val newStats = t.stats.mapConserve {
            case x: ValDef if valsToDrop.contains(x) => EmptyTree
            case t: Assign => assignsToReplace.get(t) match {
              case Some(vd) =>
                val newD = vd.symbol.asSymDenotation.copySymDenotation(initFlags = vd.symbol.flags.&~(Mutable))
                newD.installAfter(this)
                ValDef(vd.symbol.asTerm, t.rhs)
              case None => t
            }
            case x => x
          }

          if (newStats eq t.stats) t
          else tpd.cpy.Block(t)(newStats, t.expr)
        case tree => tree
      }

      transformation
    }
    ("valify", BeforeAndAfterErasure, visitor, transformer)
  }

  /** Inline vals */
  val devalify: Optimization = { implicit ctx: Context =>
    val timesUsed = mutable.HashMap[Symbol, Int]()
    val timesUsedAsType = mutable.HashMap[Symbol, Int]()

    val defined = mutable.HashSet[Symbol]()
    val usedInInnerClass = mutable.HashMap[Symbol, Int]()
    // Either a duplicate or a read through series of immutable fields
    val copies = mutable.HashMap[Symbol, Tree]()
    def visitType(tp: Type): Unit = {
          tp.foreachPart(x => x match {
            case TermRef(NoPrefix, _) =>
              val b4 = timesUsedAsType.getOrElseUpdate(x.termSymbol, 0)
              timesUsedAsType.put(x.termSymbol, b4 + 1)
            case _ =>
          })
    }
    def doVisit(tree: Tree, used: mutable.HashMap[Symbol, Int]): Unit = tree match {
      case valdef: ValDef if !valdef.symbol.is(Param | Mutable | Module | Lazy) &&
                             valdef.symbol.exists && !valdef.symbol.owner.isClass =>
        defined += valdef.symbol

        dropCasts(valdef.rhs) match {
          case t: Tree if readingOnlyVals(t) =>
            copies.put(valdef.symbol, valdef.rhs)
          case _ =>
        }
        visitType(valdef.symbol.info)
      case t: New =>
        val symIfExists = t.tpt.tpe.normalizedPrefix.termSymbol
        val b4 = used.getOrElseUpdate(symIfExists, 0)
        used.put(symIfExists, b4 + 1)

      case valdef: ValDef if valdef.symbol.exists && !valdef.symbol.owner.isClass &&
                             !valdef.symbol.is(Param | Module | Lazy) =>
        // TODO: handle params after constructors. Start changing public signatures by eliminating unused arguments.
        defined += valdef.symbol

      case valdef: ValDef => visitType(valdef.symbol.info)
      case t: DefDef      => visitType(t.symbol.info)
      case t: Typed       =>
        visitType(t.tpt.tpe)
      case t: TypeApply   => t.args.foreach(x => visitType(x.tpe))
      case t: RefTree =>
        val b4 = used.getOrElseUpdate(t.symbol, 0)
        used.put(t.symbol, b4 + 1)
      case _ =>
    }

    val visitor: Visitor = { tree =>
      def crossingClassBoundaries(t: Tree): Boolean = t match {
        case _: New      => true
        case _: Template => true
        case _           => false
      }
      // We shouldn't inline `This` nodes, which we approximate by not inlining
      // anything across class boundaries. To do so, we visit every class a
      // second time and record what's used in the usedInInnerClass Set.
      if (crossingClassBoundaries(tree)) {
        // Doing a foreachSubTree(tree) here would work, but would also
        // be exponential for deeply nested classes. Instead we do a short
        // circuit traversal that doesn't visit further nested classes.
        val reVisitClass = new TreeAccumulator[Unit] {
          def apply(u: Unit, t: Tree)(implicit ctx: Context): Unit = {
            doVisit(t, usedInInnerClass)
            if (!crossingClassBoundaries(t))
              foldOver((), t)
          }
        }
        reVisitClass.foldOver((), tree)
      }
      doVisit(tree, timesUsed)
    }

    val transformer: Transformer = () => localCtx => {
      val valsToDrop = defined -- timesUsed.keySet -- timesUsedAsType.keySet
      val copiesToReplaceAsDuplicates = copies.filter { x =>
        val rhs = dropCasts(x._2)
        rhs.isInstanceOf[Literal] || (!rhs.symbol.owner.isClass && !rhs.symbol.is(Method | Mutable))
      } -- timesUsedAsType.keySet
      // TODO: if a non-synthetic val is duplicate of a synthetic one, rename a synthetic one and drop synthetic flag?

      val copiesToReplaceAsUsedOnce =
        timesUsed.filter(x => x._2 == 1).
          flatMap(x => copies.get(x._1) match {
            case Some(tr) => List((x._1, tr))
            case None => Nil
          }) -- timesUsedAsType.keySet

      val replacements = copiesToReplaceAsDuplicates ++ copiesToReplaceAsUsedOnce -- usedInInnerClass.keySet

      val deepReplacer = new TreeMap() {
        override def transform(tree: Tree)(implicit ctx: Context): Tree = {
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
          // TODO: Could emit a warning for non synthetic code? This valdef is
          // probably something users would want to remove from source...
          simplify.println(s"Dropping definition of ${t.symbol.showFullName} as not used")
          t.rhs.changeOwner(t.symbol, t.symbol.owner)
        case t: ValDef if replacements.contains(t.symbol) =>
          simplify.println(s"Dropping definition of ${t.symbol.showFullName} as an alias")
          EmptyTree
        case t: New =>
          val symIfExists = t.tpt.tpe.normalizedPrefix.termSymbol
          if (replacements.contains(symIfExists)) {
            val newPrefix = deepReplacer.transform(replacements(symIfExists))
            val newTpt = t.tpt.tpe match {
              case t: NamedType =>
                t.derivedSelect(newPrefix.tpe)
            }
            New(newTpt)
          }
          else t
        case t: RefTree if !t.symbol.is(Method | Param | Mutable) =>
          if (replacements.contains(t.symbol))
            deepReplacer.transform(replacements(t.symbol)).ensureConforms(t.tpe.widen)
          else t
        case tree => tree
      }

      transformation
    }
    // See tests/pos/devalify.scala for examples of why this needs to be after Erasure.
    ("devalify", BeforeAndAfterErasure, visitor, transformer)
  }

  /** Inline val with exactly one assignment to a var. For example:
   *
   *  {
   *    val l = <expr>
   *    var r = l
   *    // code not using l
   *  }
   *
   *  becomes:
   *
   *  {
   *    var r = <expr>
   *    // code not using l
   *  }
   */
  val varify: Optimization = { implicit ctx: Context =>
    val paramsTimesUsed = mutable.HashMap[Symbol, Int]()
    val possibleRenames = mutable.HashMap[Symbol, Set[Symbol]]()
    val visitor: Visitor = {
      case t: ValDef
        if t.symbol.is(Param) =>
        paramsTimesUsed += (t.symbol -> 0)
      case valDef: ValDef
        if valDef.symbol.is(Mutable) =>
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
      val renames: Map[Symbol, Symbol] = possibleRenames.iterator
        .map(kv => (kv._1, kv._2.intersect(paramCandidates)))
        .filter(x => x._2.nonEmpty)
        .map(x => (x._1, x._2.head))
        .toMap
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
          t.symbol.flags = Mutable
          t
        case t => t
      }
      transformation
    }
    ("varify", AfterErasure, visitor, transformer)
  }

  private def unzip4[A, B, C, D](seq: Seq[(A, B, C, D)]): (Seq[A], Seq[B], Seq[C], Seq[D]) = {
    val listBuilderA = new mutable.ListBuffer[A]()
    val listBuilderB = new mutable.ListBuffer[B]()
    val listBuilderC = new mutable.ListBuffer[C]()
    val listBuilderD = new mutable.ListBuffer[D]()
    seq.foreach { x =>
      listBuilderA += x._1
      listBuilderB += x._2
      listBuilderC += x._3
      listBuilderD += x._4
    }
    (listBuilderA.toList, listBuilderB.toList, listBuilderC.toList, listBuilderD.toList)
  }
}
