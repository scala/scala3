package dotty.tools.dotc
package transform

import core.Phases._
import core.DenotTransformers._
import core.Denotations._
import core.SymDenotations._
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Names._
import core.StdNames._
import core.NameOps._
import core.NameKinds.AdaptedClosureName
import core.Decorators._
import core.Constants._
import core.Definitions._
import typer.NoChecking
import typer.ProtoTypes._
import typer.ErrorReporting._
import core.TypeErasure._
import core.Decorators._
import dotty.tools.dotc.ast.{Trees, tpd, untpd}
import ast.Trees._
import scala.collection.mutable.ListBuffer
import dotty.tools.dotc.core.{Constants, Flags}
import ValueClasses._
import TypeUtils._
import ExplicitOuter._
import core.Mode
import core.PhantomErasure
import reporting.trace
import dotty.tools.dotc.core.Annotations.Annotation
import dotty.tools.dotc.core.classfile.ClassfileConstants
import dotty.tools.dotc.core.TypeApplications.TypeParamInfo
import java.lang.StringBuilder

class Erasure extends Phase with DenotTransformer {

  override def phaseName: String = "erasure"

  /** List of names of phases that should precede this phase */
  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[InterceptedMethods], classOf[Splitter], classOf[ElimRepeated])

  override def changesMembers: Boolean = true   // the phase adds bridges
  override def changesParents: Boolean = true // the phase drops Any

  def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = ref match {
    case ref: SymDenotation =>
      def isCompacted(sym: Symbol) =
        sym.isAnonymousFunction && {
          sym.info(ctx.withPhase(ctx.phase.next)) match {
            case MethodType(nme.ALLARGS :: Nil) => true
            case _                              => false
          }
        }

      assert(ctx.phase == this, s"transforming $ref at ${ctx.phase}")
      if (ref.symbol eq defn.ObjectClass) {
        // After erasure, all former Any members are now Object members
        val ClassInfo(pre, _, ps, decls, selfInfo) = ref.info
        val extendedScope = decls.cloneScope
        for (decl <- defn.AnyClass.classInfo.decls)
          if (!decl.isConstructor) extendedScope.enter(decl)
        ref.copySymDenotation(
          info = transformInfo(ref.symbol,
              ClassInfo(pre, defn.ObjectClass, ps, extendedScope, selfInfo))
        )
      }
      else {
        val oldSymbol = ref.symbol
        val newSymbol =
          if ((oldSymbol.owner eq defn.AnyClass) && oldSymbol.isConstructor)
            defn.ObjectClass.primaryConstructor
        else oldSymbol
        val oldOwner = ref.owner
        val newOwner = if (oldOwner eq defn.AnyClass) defn.ObjectClass else oldOwner
        val oldInfo = ref.info
        val newInfo = transformInfo(oldSymbol, oldInfo)
        val oldFlags = ref.flags
        val newFlags =
          if (oldSymbol.is(Flags.TermParam) && isCompacted(oldSymbol.owner)) oldFlags &~ Flags.Param
          else oldFlags &~ Flags.HasDefaultParams // HasDefaultParams needs to be dropped because overriding might become overloading

        // TODO: define derivedSymDenotation?
        if ((oldSymbol eq newSymbol) && (oldOwner eq newOwner) && (oldInfo eq newInfo) && (oldFlags == newFlags)) ref
        else {
          assert(!ref.is(Flags.PackageClass), s"trans $ref @ ${ctx.phase} oldOwner = $oldOwner, newOwner = $newOwner, oldInfo = $oldInfo, newInfo = $newInfo ${oldOwner eq newOwner} ${oldInfo eq newInfo}")
          ref.copySymDenotation(symbol = newSymbol, owner = newOwner, initFlags = newFlags, info = newInfo)
        }
      }
    case ref =>
      ref.derivedSingleDenotation(ref.symbol, transformInfo(ref.symbol, ref.info))
  }

  val eraser = new Erasure.Typer

  def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    unit.tpdTree = eraser.typedExpr(unit.tpdTree)(ctx.fresh.setPhase(this.next))
  }

  override def checkPostCondition(tree: tpd.Tree)(implicit ctx: Context) = {
    assertErased(tree)
    tree match {
      case res: tpd.This =>
        assert(!ExplicitOuter.referencesOuter(ctx.owner.lexicallyEnclosingClass, res),
          i"Reference to $res from ${ctx.owner.showLocated}")
      case ret: tpd.Return =>
        // checked only after erasure, as checking before erasure is complicated
        // due presence of type params in returned types
        val from = if (ret.from.isEmpty) ctx.owner.enclosingMethod else ret.from.symbol
        val rType = from.info.finalResultType
        assert(ret.expr.tpe <:< rType,
          i"Returned value:${ret.expr}  does not conform to result type(${ret.expr.tpe.widen} of method $from")
      case _ =>
    }
  }

  /** Assert that tree type and its widened underlying type are erased.
   *  Also assert that term refs have fixed symbols (so we are sure
   *  they need not be reloaded using member; this would likely fail as signatures
   *  may change after erasure).
   */
  def assertErased(tree: tpd.Tree)(implicit ctx: Context): Unit = {
    assertErased(tree.typeOpt, tree)
    if (!defn.isPolymorphicAfterErasure(tree.symbol))
      assertErased(tree.typeOpt.widen, tree)
    if (ctx.mode.isExpr)
      tree.tpe match {
        case ref: TermRef =>
          assert(ref.denot.isInstanceOf[SymDenotation] ||
              ref.denot.isInstanceOf[UniqueRefDenotation],
            i"non-sym type $ref of class ${ref.getClass} with denot of class ${ref.denot.getClass} of $tree")
        case _ =>
      }
  }

  def assertErased(tp: Type, tree: tpd.Tree = tpd.EmptyTree)(implicit ctx: Context): Unit =
    if (tp.typeSymbol == defn.ArrayClass &&
        ctx.compilationUnit.source.file.name == "Array.scala") {} // ok
    else
      assert(isErasedType(tp),
        i"The type $tp - ${tp.toString} of class ${tp.getClass} of tree $tree : ${tree.tpe} / ${tree.getClass} is illegal after erasure, phase = ${ctx.phase.prev}")
}

object Erasure {
  import tpd._
  import TypeTestsCasts._

  object Boxing {

    def isUnbox(sym: Symbol)(implicit ctx: Context) =
      sym.name == nme.unbox && sym.owner.linkedClass.isPrimitiveValueClass

    def isBox(sym: Symbol)(implicit ctx: Context) =
      sym.name == nme.box && sym.owner.linkedClass.isPrimitiveValueClass

    def boxMethod(cls: ClassSymbol)(implicit ctx: Context) =
      cls.linkedClass.info.member(nme.box).symbol
    def unboxMethod(cls: ClassSymbol)(implicit ctx: Context) =
      cls.linkedClass.info.member(nme.unbox).symbol

    /** Isf this tree is an unbox operation which can be safely removed
     *  when enclosed in a box, the unboxed argument, otherwise EmptyTree.
     *  Note that one can't always remove a Box(Unbox(x)) combination because the
     *  process of unboxing x may lead to throwing an exception.
     *  This is important for specialization: calls to the super constructor should not box/unbox specialized
     *  fields (see TupleX). (ID)
     */
    private def safelyRemovableUnboxArg(tree: Tree)(implicit ctx: Context): Tree = tree match {
      case Apply(fn, arg :: Nil)
      if isUnbox(fn.symbol) && defn.ScalaBoxedClasses().contains(arg.tpe.widen.typeSymbol) =>
        arg
      case _ =>
        EmptyTree
    }

    def constant(tree: Tree, const: Tree)(implicit ctx: Context) =
      (if (isPureExpr(tree)) const else Block(tree :: Nil, const))
        .withPos(tree.pos)

    final def box(tree: Tree, target: => String = "")(implicit ctx: Context): Tree = trace(i"boxing ${tree.showSummary}: ${tree.tpe} into $target") {
      tree.tpe.widen match {
        case ErasedValueType(tycon, _) =>
          New(tycon, cast(tree, underlyingOfValueClass(tycon.symbol.asClass)) :: Nil) // todo: use adaptToType?
        case tp =>
          val cls = tp.classSymbol
          if (cls eq defn.UnitClass) constant(tree, ref(defn.BoxedUnit_UNIT))
          else if (cls eq defn.NothingClass) tree // a non-terminating expression doesn't need boxing
          else {
            assert(cls ne defn.ArrayClass)
            val arg = safelyRemovableUnboxArg(tree)
            if (arg.isEmpty) ref(boxMethod(cls.asClass)).appliedTo(tree)
            else {
              ctx.log(s"boxing an unbox: ${tree.symbol} -> ${arg.tpe}")
              arg
            }
          }
      }
    }

    def unbox(tree: Tree, pt: Type)(implicit ctx: Context): Tree = trace(i"unboxing ${tree.showSummary}: ${tree.tpe} as a $pt") {
      pt match {
        case ErasedValueType(tycon, underlying) =>
          def unboxedTree(t: Tree) =
            adaptToType(t, tycon)
            .select(valueClassUnbox(tycon.symbol.asClass))
            .appliedToNone

          // Null unboxing needs to be treated separately since we cannot call a method on null.
          // "Unboxing" null to underlying is equivalent to doing null.asInstanceOf[underlying]
          // See tests/pos/valueclasses/nullAsInstanceOfVC.scala for cases where this might happen.
          val tree1 =
            if (tree.tpe isRef defn.NullClass)
              adaptToType(tree, underlying)
            else if (wasPhantom(underlying))
              PhantomErasure.erasedParameterRef
            else if (!(tree.tpe <:< tycon)) {
              assert(!(tree.tpe.typeSymbol.isPrimitiveValueClass))
              val nullTree = Literal(Constant(null))
              val unboxedNull = adaptToType(nullTree, underlying)

              evalOnce(tree) { t =>
                If(t.select(defn.Object_eq).appliedTo(nullTree),
                  unboxedNull,
                  unboxedTree(t))
              }
            } else unboxedTree(tree)

          cast(tree1, pt)
        case _ =>
          val cls = pt.widen.classSymbol
          if (cls eq defn.UnitClass) constant(tree, Literal(Constant(())))
          else {
            assert(cls ne defn.ArrayClass)
            ref(unboxMethod(cls.asClass)).appliedTo(tree)
          }
      }
    }

    /** Generate a synthetic cast operation from tree.tpe to pt.
     *  Does not do any boxing/unboxing (this is handled upstream).
     *  Casts from and to ErasedValueType are special, see the explanation
     *  in ExtensionMethods#transform.
     */
    def cast(tree: Tree, pt: Type)(implicit ctx: Context): Tree = trace(i"cast ${tree.tpe.widen} --> $pt", show = true) {
      def wrap(tycon: TypeRef) =
        ref(u2evt(tycon.typeSymbol.asClass)).appliedTo(tree)
      def unwrap(tycon: TypeRef) =
        ref(evt2u(tycon.typeSymbol.asClass)).appliedTo(tree)


      assert(!pt.isInstanceOf[SingletonType], pt)
      if (pt isRef defn.UnitClass) unbox(tree, pt)
      else (tree.tpe.widen, pt) match {
        case (JavaArrayType(treeElem), JavaArrayType(ptElem))
        if treeElem.widen.isPrimitiveValueType && !ptElem.isPrimitiveValueType =>
          // See SI-2386 for one example of when this might be necessary.
          cast(ref(defn.runtimeMethodRef(nme.toObjectArray)).appliedTo(tree), pt)

        // When casting between two EVTs, we need to check which one underlies the other to determine
        // wheter u2evt or evt2u should be used.
        case (tp1 @ ErasedValueType(tycon1, underlying1), tp2 @ ErasedValueType(tycon2, underlying2)) =>
          if (tp1 <:< underlying2)
            // Cast EVT(tycon1, underlying1) to EVT(tycon2, EVT(tycon1, underlying1))
            wrap(tycon2)
          else {
            assert(underlying1 <:< tp2, i"Non-sensical cast between unrelated types $tp1 and $tp2")
            // Cast EVT(tycon1, EVT(tycon2, underlying2)) to EVT(tycon2, underlying2)
            unwrap(tycon1)
          }

        // When only one type is an EVT then we already know that the other one is the underlying
        case (_, ErasedValueType(tycon2, _)) =>
          wrap(tycon2)
        case (ErasedValueType(tycon1, _), _) =>
          unwrap(tycon1)

        case _ =>
          if (pt.isPrimitiveValueType)
            primitiveConversion(tree, pt.classSymbol)
          else
            tree.asInstance(pt)
      }
    }

    /** Adaptation of an expression `e` to an expected type `PT`, applying the following
     *  rewritings exhaustively as long as the type of `e` is not a subtype of `PT`.
     *
     *    e -> e()           if `e` appears not as the function part of an application
     *    e -> box(e)        if `e` is of erased value type
     *    e -> unbox(e, PT)  otherwise, if `PT` is an erased value type
     *    e -> box(e)        if `e` is of primitive type and `PT` is not a primitive type
     *    e -> unbox(e, PT)  if `PT` is a primitive type and `e` is not of primitive type
     *    e -> cast(e, PT)   otherwise
     */
    def adaptToType(tree: Tree, pt: Type)(implicit ctx: Context): Tree =
      if (pt.isInstanceOf[FunProto]) tree
      else tree.tpe.widen match {
        case MethodType(Nil) if tree.isTerm =>
          adaptToType(tree.appliedToNone, pt)
        case tpw =>
          if (pt.isInstanceOf[ProtoType] || tree.tpe <:< pt)
            tree
          else if (tpw.isErasedValueType)
            adaptToType(box(tree), pt)
          else if (pt.isErasedValueType)
            adaptToType(unbox(tree, pt), pt)
          else if (tpw.isPrimitiveValueType && !pt.isPrimitiveValueType)
            adaptToType(box(tree), pt)
          else if (pt.isPrimitiveValueType && !tpw.isPrimitiveValueType)
            adaptToType(unbox(tree, pt), pt)
          else
            cast(tree, pt)
      }
  }

  class Typer extends typer.ReTyper with NoChecking {
    import Boxing._

    def erasedType(tree: untpd.Tree)(implicit ctx: Context): Type = {
      val tp = tree.typeOpt
      if (tree.isTerm) erasedRef(tp) else valueErasure(tp)
    }

    override def promote(tree: untpd.Tree)(implicit ctx: Context): tree.ThisTree[Type] = {
      assert(tree.hasType)
      val erased = erasedType(tree)
      ctx.log(s"promoting ${tree.show}: ${erased.showWithUnderlying()}")
      tree.withType(erased)
    }

    /** When erasing most TypeTrees we should not semi-erase value types.
     *  This is not the case for [[DefDef#tpt]], [[ValDef#tpt]] and [[Typed#tpt]], they
     *  are handled separately by [[typedDefDef]], [[typedValDef]] and [[typedTyped]].
     */
    override def typedTypeTree(tree: untpd.TypeTree, pt: Type)(implicit ctx: Context): TypeTree =
      tree.withType(erasure(tree.tpe))

    /** This override is only needed to semi-erase type ascriptions */
    override def typedTyped(tree: untpd.Typed, pt: Type)(implicit ctx: Context): Tree = {
      val Typed(expr, tpt) = tree
      val tpt1 = promote(tpt)
      val expr1 = typed(expr, tpt1.tpe)
      assignType(untpd.cpy.Typed(tree)(expr1, tpt1), tpt1)
    }

    override def typedLiteral(tree: untpd.Literal)(implicit ctx: Context): Literal =
      if (tree.typeOpt.isRef(defn.UnitClass)) tree.withType(tree.typeOpt)
      else if (tree.const.tag == Constants.ClazzTag) Literal(Constant(erasure(tree.const.typeValue)))
      else super.typedLiteral(tree)

    /** Type check select nodes, applying the following rewritings exhaustively
     *  on selections `e.m`, where `OT` is the type of the owner of `m` and `ET`
     *  is the erased type of the selection's original qualifier expression.
     *
     *      e.m1 -> e.m2          if `m1` is a member of Any or AnyVal and `m2` is
     *                            the same-named member in Object.
     *      e.m -> box(e).m       if `e` is primitive and `m` is a member or a reference class
     *                            or `e` has an erased value class type.
     *      e.m -> unbox(e).m     if `e` is not primitive and `m` is a member of a primtive type.
     *      e.m -> cast(e, OT).m  if the type of `e` does not conform to OT and `m`
     *                            is not an array operation.
     *
     *  If `m` is an array operation, i.e. one of the members apply, update, length, clone, and
     *  <init> of class Array, we additionally try the following rewritings:
     *
     *      e.m -> runtime.array_m(e)   if ET is Object
     *      e.m -> cast(e, ET).m        if the type of `e` does not conform to ET
     *      e.clone -> e.clone'         where clone' is Object's clone method
     *      e.m -> e.[]m                if `m` is an array operation other than `clone`.
     */
    override def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = {

      def mapOwner(sym: Symbol): Symbol = {
        def recur(owner: Symbol): Symbol =
          if ((owner eq defn.AnyClass) || (owner eq defn.AnyValClass)) {
            assert(sym.isConstructor, s"${sym.showLocated}")
            defn.ObjectClass
          } else if (defn.isSyntheticFunctionClass(owner))
            defn.erasedFunctionClass(owner)
          else
            owner
        recur(sym.owner)
      }

      val origSym = tree.symbol
      val owner = mapOwner(origSym)
      val sym = if (owner eq origSym.owner) origSym else owner.info.decl(origSym.name).symbol
      assert(sym.exists, origSym.showLocated)

      def select(qual: Tree, sym: Symbol): Tree =
        untpd.cpy.Select(tree)(qual, sym.name).withType(NamedType(qual.tpe, sym))

      def selectArrayMember(qual: Tree, erasedPre: Type): Tree =
        if (erasedPre isRef defn.ObjectClass)
          runtimeCallWithProtoArgs(tree.name.genericArrayOp, pt, qual)
        else if (!(qual.tpe <:< erasedPre))
          selectArrayMember(cast(qual, erasedPre), erasedPre)
        else
          assignType(untpd.cpy.Select(tree)(qual, tree.name.primitiveArrayOp), qual)

      def adaptIfSuper(qual: Tree): Tree = qual match {
        case Super(thisQual, untpd.EmptyTypeIdent) =>
          val SuperType(thisType, supType) = qual.tpe
          if (sym.owner is Flags.Trait)
            cpy.Super(qual)(thisQual, untpd.Ident(sym.owner.asClass.name))
              .withType(SuperType(thisType, sym.owner.typeRef))
          else
            qual.withType(SuperType(thisType, thisType.firstParent.typeConstructor))
        case _ =>
          qual
      }

      def recur(qual: Tree): Tree = {
        val qualIsPrimitive = qual.tpe.widen.isPrimitiveValueType
        val symIsPrimitive = sym.owner.isPrimitiveValueClass
        if (qualIsPrimitive && !symIsPrimitive || qual.tpe.widenDealias.isErasedValueType)
          recur(box(qual))
        else if (!qualIsPrimitive && symIsPrimitive)
          recur(unbox(qual, sym.owner.typeRef))
        else if (sym.owner eq defn.ArrayClass)
          selectArrayMember(qual, erasure(tree.qualifier.typeOpt.widen.finalResultType))
        else {
          val qual1 = adaptIfSuper(qual)
          if (qual1.tpe.derivesFrom(sym.owner) || qual1.isInstanceOf[Super])
            select(qual1, sym)
          else
            recur(cast(qual1, sym.owner.typeRef))
        }
      }

      if ((origSym eq defn.Phantom_assume) || (origSym.is(Flags.ParamAccessor) && wasPhantom(pt)))
        PhantomErasure.erasedAssume
      else recur(typed(tree.qualifier, AnySelectionProto))
    }

    override def typedIdent(tree: untpd.Ident, pt: Type)(implicit ctx: Context): tpd.Tree =
      if (tree.symbol eq defn.Phantom_assume) PhantomErasure.erasedAssume
      else if (tree.symbol.is(Flags.Param) && wasPhantom(tree.typeOpt)) PhantomErasure.erasedParameterRef
      else super.typedIdent(tree, pt)

    override def typedThis(tree: untpd.This)(implicit ctx: Context): Tree =
      if (tree.symbol == ctx.owner.lexicallyEnclosingClass || tree.symbol.isStaticOwner) promote(tree)
      else {
        ctx.log(i"computing outer path from ${ctx.owner.ownersIterator.toList}%, % to ${tree.symbol}, encl class = ${ctx.owner.enclosingClass}")
        outer.path(toCls = tree.symbol)
      }

    private def runtimeCallWithProtoArgs(name: Name, pt: Type, args: Tree*)(implicit ctx: Context): Tree = {
      val meth = defn.runtimeMethodRef(name)
      val followingParams = meth.symbol.info.firstParamTypes.drop(args.length)
      val followingArgs = protoArgs(pt).zipWithConserve(followingParams)(typedExpr).asInstanceOf[List[tpd.Tree]]
      ref(meth).appliedToArgs(args.toList ++ followingArgs)
    }

    private def protoArgs(pt: Type): List[untpd.Tree] = pt match {
      case pt: FunProto => pt.args ++ protoArgs(pt.resType)
      case _ => Nil
    }

    override def typedTypeApply(tree: untpd.TypeApply, pt: Type)(implicit ctx: Context) = {
      val ntree = interceptTypeApply(tree.asInstanceOf[TypeApply])(ctx.withPhase(ctx.erasurePhase))

      ntree match {
        case TypeApply(fun, args) =>
          val fun1 = typedExpr(fun, WildcardType)
          fun1.tpe.widen match {
            case funTpe: PolyType =>
              val args1 = args.mapconserve(typedType(_))
              untpd.cpy.TypeApply(tree)(fun1, args1).withType(funTpe.instantiate(args1.tpes))
            case _ => fun1
          }
        case _ => typedExpr(ntree, pt)
      }
    }

	/** Besides normal typing, this method collects all arguments
	 *  to a compacted function into a single argument of array type.
	 */
    override def typedApply(tree: untpd.Apply, pt: Type)(implicit ctx: Context): Tree = {
      val Apply(fun, args) = tree
      if (fun.symbol == defn.cbnArg)
        typedUnadapted(args.head, pt)
      else typedExpr(fun, FunProto(args, pt, this)) match {
        case fun1: Apply => // arguments passed in prototype were already passed
          fun1
        case fun1 =>
          fun1.tpe.widen match {
            case mt: MethodType =>
              val outers = outer.args(fun.asInstanceOf[tpd.Tree]) // can't use fun1 here because its type is already erased
              var args0 = outers ::: args ++ protoArgs(pt)
              if (args0.length > MaxImplementedFunctionArity && mt.paramInfos.length == 1) {
                val bunchedArgs = untpd.JavaSeqLiteral(args0, TypeTree(defn.ObjectType))
                  .withType(defn.ArrayOf(defn.ObjectType))
                args0 = bunchedArgs :: Nil
              }
              // Arguments are phantom if an only if the parameters are phantom, guaranteed by the separation of type lattices
              val args1 = args0.filterConserve(arg => !wasPhantom(arg.typeOpt)).zipWithConserve(mt.paramInfos)(typedExpr)
              untpd.cpy.Apply(tree)(fun1, args1) withType mt.resultType
            case _ =>
              throw new MatchError(i"tree $tree has unexpected type of function ${fun1.tpe.widen}, was ${fun.typeOpt.widen}")
          }
      }
    }

    // The following four methods take as the proto-type the erasure of the pre-existing type,
    // if the original proto-type is not a value type.
    // This makes all branches be adapted to the correct type.
    override def typedSeqLiteral(tree: untpd.SeqLiteral, pt: Type)(implicit ctx: Context) =
      super.typedSeqLiteral(tree, erasure(tree.typeOpt))
        // proto type of typed seq literal is original type;

    override def typedIf(tree: untpd.If, pt: Type)(implicit ctx: Context) =
      super.typedIf(tree, adaptProto(tree, pt))

    override def typedMatch(tree: untpd.Match, pt: Type)(implicit ctx: Context) =
      super.typedMatch(tree, adaptProto(tree, pt))

    override def typedTry(tree: untpd.Try, pt: Type)(implicit ctx: Context) =
      super.typedTry(tree, adaptProto(tree, pt))

    private def adaptProto(tree: untpd.Tree, pt: Type)(implicit ctx: Context) = {
      if (pt.isValueType) pt else {
        if (tree.typeOpt.derivesFrom(ctx.definitions.UnitClass))
          tree.typeOpt
        else valueErasure(tree.typeOpt)
      }
    }

    override def typedValDef(vdef: untpd.ValDef, sym: Symbol)(implicit ctx: Context): ValDef =
      super.typedValDef(untpd.cpy.ValDef(vdef)(
        tpt = untpd.TypedSplice(TypeTree(sym.info).withPos(vdef.tpt.pos))), sym)

    /** Besides normal typing, this function also compacts anonymous functions
     *  with more than `MaxImplementedFunctionArity` parameters to ise a single
     *  parameter of type `[]Object`.
     */
    override def typedDefDef(ddef: untpd.DefDef, sym: Symbol)(implicit ctx: Context) = {
      val restpe =
        if (sym.isConstructor) defn.UnitType
        else sym.info.resultType
      var vparamss1 = (outer.paramDefs(sym) ::: ddef.vparamss.flatten) :: Nil
      var rhs1 = ddef.rhs match {
        case id @ Ident(nme.WILDCARD) => untpd.TypedSplice(id.withType(restpe))
        case _ => ddef.rhs
      }
      if (sym.isAnonymousFunction && vparamss1.head.length > MaxImplementedFunctionArity) {
        val bunchedParam = ctx.newSymbol(sym, nme.ALLARGS, Flags.TermParam, JavaArrayType(defn.ObjectType))
        def selector(n: Int) = ref(bunchedParam)
          .select(defn.Array_apply)
          .appliedTo(Literal(Constant(n)))
        val paramDefs = vparamss1.head.zipWithIndex.map {
          case (paramDef, idx) =>
            assignType(untpd.cpy.ValDef(paramDef)(rhs = selector(idx)), paramDef.symbol)
        }
        vparamss1 = (tpd.ValDef(bunchedParam) :: Nil) :: Nil
        rhs1 = untpd.Block(paramDefs, rhs1)
      }
      vparamss1 = vparamss1.mapConserve(_.filterConserve(vparam => !wasPhantom(vparam.tpe)))
      if (sym.is(Flags.ParamAccessor) && wasPhantom(ddef.tpt.tpe)) {
        sym.resetFlag(Flags.ParamAccessor)
        rhs1 = PhantomErasure.erasedParameterRef
      }
      val ddef1 = untpd.cpy.DefDef(ddef)(
        tparams = Nil,
        vparamss = vparamss1,
        tpt = untpd.TypedSplice(TypeTree(restpe).withPos(ddef.tpt.pos)),
        rhs = rhs1)
      super.typedDefDef(ddef1, sym)
    }

    override def typedClosure(tree: untpd.Closure, pt: Type)(implicit ctx: Context) = {
      val xxl = defn.isXXLFunctionClass(tree.typeOpt.typeSymbol)
      var implClosure @ Closure(_, meth, _) = super.typedClosure(tree, pt)
      if (xxl) implClosure = cpy.Closure(implClosure)(tpt = TypeTree(defn.FunctionXXLType))
      implClosure.tpe match {
        case SAMType(sam) =>
          val implType = meth.tpe.widen.asInstanceOf[MethodType]

          val implParamTypes = implType.paramInfos
          val List(samParamTypes) = sam.info.paramInfoss
          val implResultType = implType.resultType
          val samResultType = sam.info.resultType

          // The following code:
          //
          //     val f: Function1[Int, Any] = x => ...
          //
          // results in the creation of a closure and a method in the typer:
          //
          //     def $anonfun(x: Int): Any = ...
          //     val f: Function1[Int, Any] = closure($anonfun)
          //
          // Notice that `$anonfun` takes a primitive as argument, but the single abstract method
          // of `Function1` after erasure is:
          //
          //     def apply(x: Object): Object
          //
          // which takes a reference as argument. Hence, some form of adaptation is required.
          //
          // If we do nothing, the LambdaMetaFactory bootstrap method will
          // automatically do the adaptation. Unfortunately, the result does not
          // implement the expected Scala semantics: null should be "unboxed" to
          // the default value of the value class, but LMF will throw a
          // NullPointerException instead. LMF is also not capable of doing
          // adaptation for derived value classes.
          //
          // Thus, we need to replace the closure method by a bridge method that
          // forwards to the original closure method with appropriate
          // boxing/unboxing. For our example above, this would be:
          //
          //     def $anonfun1(x: Object): Object = $anonfun(BoxesRunTime.unboxToInt(x))
          //     val f: Function1 = closure($anonfun1)
          //
          // In general, a bridge is needed when, after Erasure:
          // - one of the parameter type of the closure method is a non-reference type,
          //   and the corresponding type in the SAM is a reference type
          // - or the result type of the closure method is an erased value type
          //   and the result type in the SAM isn't
          // However, the following exception exists: If the SAM is replaced by
          // JFunction*mc* in [[FunctionalInterfaces]], no bridge is needed: the
          // SAM contains default methods to handle adaptation
          //
          // See test cases lambda-*.scala and t8017/ for concrete examples.

          def isReferenceType(tp: Type) = !tp.isPrimitiveValueType && !tp.isErasedValueType

          if (!defn.isSpecializableFunction(implClosure.tpe.widen.classSymbol.asClass, implParamTypes, implResultType)) {
            val paramAdaptationNeeded =
              (implParamTypes, samParamTypes).zipped.exists((implType, samType) =>
                !isReferenceType(implType) && isReferenceType(samType))
            val resultAdaptationNeeded =
              implResultType.isErasedValueType && !samResultType.isErasedValueType

            if (paramAdaptationNeeded || resultAdaptationNeeded) {
              val bridgeType =
                if (paramAdaptationNeeded) {
                  if (resultAdaptationNeeded) sam.info
                  else implType.derivedLambdaType(paramInfos = samParamTypes)
                } else implType.derivedLambdaType(resType = samResultType)
              val bridge = ctx.newSymbol(ctx.owner, AdaptedClosureName(meth.symbol.name.asTermName), Flags.Synthetic | Flags.Method, bridgeType)
              val bridgeCtx = ctx.withOwner(bridge)
              Closure(bridge, bridgeParamss => {
                implicit val ctx = bridgeCtx

                val List(bridgeParams) = bridgeParamss
                val rhs = Apply(meth, (bridgeParams, implParamTypes).zipped.map(adapt(_, _)))
                adapt(rhs, bridgeType.resultType)
              }, targetType = implClosure.tpt.tpe)
            } else implClosure
          } else implClosure
        case _ =>
          implClosure
      }
    }

    override def typedTypeDef(tdef: untpd.TypeDef, sym: Symbol)(implicit ctx: Context) =
      EmptyTree

    override def typedStats(stats: List[untpd.Tree], exprOwner: Symbol)(implicit ctx: Context): List[Tree] = {
      val stats1 =
        if (takesBridges(ctx.owner)) new Bridges(ctx.owner.asClass).add(stats)
        else stats
      super.typedStats(stats1, exprOwner).filter(!_.isEmpty)
    }

    override def adapt(tree: Tree, pt: Type)(implicit ctx: Context): Tree =
      trace(i"adapting ${tree.showSummary}: ${tree.tpe} to $pt", show = true) {
        assert(ctx.phase == ctx.erasurePhase.next, ctx.phase)
        if (tree.isEmpty) tree
        else if (ctx.mode is Mode.Pattern) tree // TODO: replace with assertion once pattern matcher is active
        else adaptToType(tree, pt)
      }
  }

  def takesBridges(sym: Symbol)(implicit ctx: Context) =
    sym.isClass && !sym.is(Flags.Trait | Flags.Package)

  private def wasPhantom(tp: Type)(implicit ctx: Context): Boolean =
    tp.widenDealias.classSymbol eq defn.ErasedPhantomClass

  /** The intersection dominator (SLS 3.7) of a list of types is computed as follows.
    *
    *  - If the list contains one or more occurrences of scala.Array with
    *    type parameters El1, El2, ... then the dominator is scala.Array with
    *    type parameter of intersectionDominator(List(El1, El2, ...)).           <--- @PP: not yet in spec.
    *  - Otherwise, the list is reduced to a subsequence containing only types
    *    which are not subtypes of other listed types (the span.)
    *  - If the span is empty, the dominator is Object.
    *  - If the span contains a class Tc which is not a trait and which is
    *    not Object, the dominator is Tc.                                        <--- @PP: "which is not Object" not in spec.
    *  - Otherwise, the dominator is the first element of the span.
    */
  private def intersectionDominator(parents: List[Type])(implicit ctx: Context): Type = {
    if (parents.isEmpty) defn.ObjectType
    else {
      val psyms = parents map (_.typeSymbol)
      if (psyms contains defn.ArrayClass) {
        // treat arrays specially
        defn.ArrayType.appliedTo(intersectionDominator(parents.filter(_.typeSymbol == defn.ArrayClass).map(t => t.typeParams.head.paramInfo)))
      } else {
        // implement new spec for erasure of refined types.
        def isUnshadowed(psym: Symbol) =
          !(psyms exists (qsym => (psym ne qsym) && (qsym isSubClass psym)))
        val cs = parents.iterator.filter { p => // isUnshadowed is a bit expensive, so try classes first
          val psym = p.typeSymbol
          psym.ensureCompleted()
          psym.isClass && !psym.is(Flags.Trait) && isUnshadowed(psym)
        }
        (if (cs.hasNext) cs else parents.iterator.filter(p => isUnshadowed(p.typeSymbol))).next()
      }
    }
  }

  /* Drop redundant types (ones which are implemented by some other parent) from the immediate parents.
   * This is important on Android because there is otherwise an interface explosion.
   */
  private def minimizeParents(cls: Symbol, parents: List[Type])(implicit ctx: Context): List[Type] = if (parents.isEmpty) parents else {
    // val requiredDirect: Symbol => Boolean = requiredDirectInterfaces.getOrElse(cls, Set.empty)
    var rest   = parents.tail
    var leaves = collection.mutable.ListBuffer.empty[Type] += parents.head
    while (rest.nonEmpty) {
      val candidate = rest.head
      val candidateSym = candidate.typeSymbol
      // val required = requiredDirect(candidateSym) || !leaves.exists(t => t.typeSymbol isSubClass candidateSym)
      val required = !leaves.exists(t => t.typeSymbol.isSubClass(candidateSym))
      if (required) {
        leaves = leaves filter { t =>
          val ts = t.typeSymbol
          !(ts.is(Flags.Trait) || ts.is(Flags.PureInterface)) || !candidateSym.isSubClass(ts)
          // requiredDirect(ts) || !ts.isTraitOrInterface || !candidateSym.isSubClass(ts)
        }
        leaves += candidate
      }
      rest = rest.tail
    }
    leaves.toList
  }

  private def hiBounds(bounds: TypeBounds)(implicit ctx: Context): List[Type] = bounds.hi.widenDealias match {
    case AndType(tp1, tp2) => tp1 :: tp2 :: Nil
    case tp => tp :: Nil
  }

  /** Arrays despite their finality may turn up as refined type parents,
    *  e.g. with "tagged types" like Array[Int] with T.
    */
  private def unboundedGenericArrayLevel(tp: Type)(implicit ctx: Context): Int = tp match {
    case GenericArray(level, core) if !(core <:< defn.AnyRefType) =>
      level
    case AndType(tp1, tp2) =>
      Math.max(unboundedGenericArrayLevel(tp1), unboundedGenericArrayLevel(tp2))
    case _ =>
      0
  }

  // only refer to type params that will actually make it into the sig, this excludes:
  // * higher-order type parameters
  // * type parameters appearing in method parameters
  // * type members not visible in an enclosing template
  private def isTypeParameterInSig(sym: Symbol, initialSymbol: Symbol)(implicit ctx: Context) = {
    !sym.isHigherOrderTypeParameter &&
      sym.isTypeParameterOrSkolem && (
      (initialSymbol.enclClassChain.exists(sym isContainedIn _)) ||
        (initialSymbol.is(Flags.Method) && initialSymbol.typeParams.contains(sym))
      )
  }

  final def javaSig(sym0: Symbol, info: Type, markClassUsed: Symbol => Unit)(implicit ctx: Context): Option[String] =
    ctx.atPhase(ctx.erasurePhase) { implicit ctx => javaSig0(sym0, info, markClassUsed) }

  @noinline
  private final def javaSig0(sym0: Symbol, info: Type, markClassUsed: Symbol => Unit)(implicit ctx: Context): Option[String] = {
    val builder = new StringBuilder(64)
    val isTraitSignature = sym0.enclosingClass.is(Flags.Trait)

    def superSig(cls: Symbol, parents: List[Type]): Unit = {
      def isInterfaceOrTrait(sym: Symbol) = sym.is(Flags.PureInterface) || sym.is(Flags.Trait)

      // a signature should always start with a class
      def ensureClassAsFirstParent(tps: List[Type]) = tps match {
        case Nil => defn.ObjectType :: Nil
        case head :: tail if isInterfaceOrTrait(head.typeSymbol) => defn.ObjectType :: tps
        case _ => tps
      }

      val minParents = minimizeParents(cls, parents)
      val validParents =
        if (isTraitSignature)
          // java is unthrilled about seeing interfaces inherit from classes
          minParents filter (p => isInterfaceOrTrait(p.typeSymbol))
        else minParents

      val ps = ensureClassAsFirstParent(validParents)
      ps.foreach(boxedSig)
    }

    def boxedSig(tp: Type): Unit = jsig(tp, primitiveOK = false)

    def boundsSig(bounds: List[Type]): Unit = {
      val (isTrait, isClass) = bounds partition (_.typeSymbol.is(Flags.Trait))
      isClass match {
        case Nil    => builder.append(':') // + boxedSig(ObjectTpe)
        case x :: _ => builder.append(':'); boxedSig(x)
      }
      isTrait.foreach { tp =>
        builder.append(':')
        boxedSig(tp)
      }
    }

    def paramSig(param: LambdaParam): Unit = {
      builder.append(sanitizeName(param.paramName))
      boundsSig(hiBounds(param.paramInfo.bounds))
    }

    def polyParamSig(tparams: List[LambdaParam]): Unit =
      if (tparams.nonEmpty) {
        builder.append('<')
        tparams.foreach(paramSig)
        builder.append('>')
      }

    def typeParamSig(name: Name): Unit = {
      builder.append(ClassfileConstants.TVAR_TAG)
      builder.append(sanitizeName(name))
      builder.append(';')
    }

    def methodResultSig(restpe: Type): Unit = {
      val finalType = restpe.finalResultType
      val sym = finalType.typeSymbol
      if (sym == defn.UnitClass || sym == defn.BoxedUnitModule || sym0.isConstructor) {
        builder.append(ClassfileConstants.VOID_TAG)
      } else {
        jsig(finalType)
      }
    }

    // This will reject any name that has characters that cannot appear in
    // names on the JVM. Interop with Java is not guaranteed for those, so we
    // dont need to generate signatures for them.
    def sanitizeName(name: Name): String = {
      val nameString = name.mangledString
      if (nameString.forall(c => c == '.' || Character.isJavaIdentifierPart(c))) {
        nameString
      } else {
        throw new UnknownSig
      }
    }

    // Anything which could conceivably be a module (i.e. isn't known to be
    // a type parameter or similar) must go through here or the signature is
    // likely to end up with Foo<T>.Empty where it needs Foo<T>.Empty$.
    def fullNameInSig(sym: Symbol): Unit = {
      val name = ctx.atPhase(ctx.genBCodePhase) { implicit ctx => sanitizeName(sym.fullName).replace('.', '/') }
      builder.append('L').append(name)
    }

    @noinline
    def jsig(tp0: Type, existentiallyBound: List[Symbol] = Nil, toplevel: Boolean = false, primitiveOK: Boolean = true): Unit = {

      val tp = tp0.dealias
      tp match {

        case ref @ TypeParamRef(_: PolyType, _) =>
          typeParamSig(ref.paramName.lastPart)

        case RefOrAppliedType(sym, pre, args) =>
          def argSig(tp: Type): Unit =
            tp match {
              case bounds: TypeBounds =>
                if (!(defn.AnyType <:< bounds.hi)) {
                  builder.append('+')
                  boxedSig(bounds.hi)
                }
                else if (!(bounds.lo <:< defn.NothingType)) {
                  builder.append('-')
                  boxedSig(bounds.lo)
                }
                else builder.append('*')
              case PolyType(_, res) =>
                builder.append('*') // scala/bug#7932
              case _: HKTypeLambda =>
                fullNameInSig(tp.typeSymbol)
                builder.append(';')
              case _ =>
                boxedSig(tp)
            }
          def classSig: Unit = {
            markClassUsed(sym)
            val preRebound = pre.baseType(sym.owner) // #2585
            if (needsJavaSig(preRebound, Nil)) {
              val i = builder.length()
              jsig(preRebound, existentiallyBound)
              if (builder.charAt(i) == 'L') {
                builder.delete(builder.length() - 1, builder.length())// delete ';'
                // If the prefix is a module, drop the '$'. Classes (or modules) nested in modules
                // are separated by a single '$' in the filename: `object o { object i }` is o$i$.
                if (preRebound.typeSymbol.is(Flags.ModuleClass))
                  builder.delete(builder.length() - 1, builder.length())

                // Ensure every '.' in the generated signature immediately follows
                // a close angle bracket '>'.  Any which do not are replaced with '$'.
                // This arises due to multiply nested classes in the face of the
                // rewriting explained at rebindInnerClass.

                // TODO revisit this. Does it align with javac for code that can be expressed in both languages?
                val delimiter = if (builder.charAt(builder.length() - 1) == '>') '.' else '$'
                builder.append(delimiter).append(sanitizeName(sym.name.asSimpleName))
              } else fullNameInSig(sym)
            } else fullNameInSig(sym)

            if (args.nonEmpty) {
              builder.append('<')
              args foreach argSig
              builder.append('>')
            }
            builder.append(';')
          }

          // If args isEmpty, Array is being used as a type constructor
          if (sym == defn.ArrayClass && args.nonEmpty) {
            if (unboundedGenericArrayLevel(tp) == 1) jsig(defn.ObjectType)
            else {
              builder.append(ClassfileConstants.ARRAY_TAG)
              args.foreach(jsig(_))
            }
          }
          else if (isTypeParameterInSig(sym, sym0)) {
            assert(!sym.isAliasType, "Unexpected alias type: " + sym)
            typeParamSig(sym.name.lastPart)
          }
          else if (sym == defn.AnyClass || sym == defn.AnyValClass || sym == defn.SingletonClass)
            jsig(defn.ObjectType)
          else if (sym == defn.UnitClass || sym == defn.BoxedUnitModule)
            jsig(defn.BoxedUnitType)
          else if (sym == defn.NothingClass)
            jsig(defn.RuntimeNothingModuleRef)
          else if (sym == defn.NullClass)
            jsig(defn.RuntimeNullModuleRef)
          else if (sym.isPrimitiveValueClass) {
            if (!primitiveOK) jsig(defn.ObjectType)
            else if (sym == defn.UnitClass) jsig(defn.BoxedUnitType)
            else builder.append(sym.info.toTag)
          }
          else if (sym.isDerivedValueClass) {
            val unboxed     = sym.derivedValueClassUnbox.info.finalResultType
            val unboxedSeen = (tp.memberInfo(sym.derivedValueClassUnbox)).finalResultType
            if (unboxedSeen.isPrimitiveValueType && !primitiveOK)
              classSig
            else
              jsig(unboxedSeen, existentiallyBound, toplevel, primitiveOK)
          }
          else if (tp.isPhantom)
            jsig(defn.ErasedPhantomType)
          else if (sym.isClass)
            classSig
          else
            jsig(erasure(tp), existentiallyBound, toplevel, primitiveOK)

        case ExprType(restpe) if toplevel =>
          builder.append("()")
          methodResultSig(restpe)

        case ExprType(restpe) =>
          jsig(defn.FunctionType(0).appliedTo(restpe))

        case PolyType(tparams, mtpe: MethodType) =>
          assert(tparams.nonEmpty)
          if (toplevel) polyParamSig(tparams)
          jsig(mtpe)

        // Nullary polymorphic method
        case PolyType(tparams, restpe) =>
          assert(tparams.nonEmpty)
          if (toplevel) polyParamSig(tparams)
          builder.append("()")
          methodResultSig(restpe)

        case mtpe: MethodType =>
          val params = mtpe.paramInfos
          val restpe = mtpe.resultType
          builder.append('(')
          // TODO: Update once we support varargs
          params.foreach { tp =>
            jsig(tp)
          }
          builder.append(')')
          methodResultSig(restpe)

        case AndType(tp1, tp2) =>
          jsig(intersectionDominator(tp1 :: tp2 :: Nil), primitiveOK = primitiveOK)

        case ci: ClassInfo =>
          def polyParamSig(tparams: List[TypeParamInfo]): Unit =
            if (tparams.nonEmpty) {
              builder.append('<')
              tparams.foreach { tp =>
                builder.append(sanitizeName(tp.paramName.lastPart))
                boundsSig(hiBounds(tp.paramInfo.bounds))
              }
              builder.append('>')
            }
          val tParams = tp.typeParams
          if (toplevel) polyParamSig(tParams)
          superSig(ci.typeSymbol, ci.parents)

        case AnnotatedType(atp, _) =>
          jsig(atp, existentiallyBound, toplevel, primitiveOK)

        case hktl: HKTypeLambda =>
          jsig(hktl.finalResultType)

        case _ =>
          val etp = erasure(tp)
          if (etp eq tp) throw new UnknownSig
          else jsig(etp)
      }
    }
    val throwsArgs = sym0.annotations flatMap ThrownException.unapply
    if (needsJavaSig(info, throwsArgs)) {
      try {
        jsig(info, toplevel = true)
        throwsArgs.foreach { t =>
          builder.append('^')
          jsig(t, toplevel = true)
        }
        Some(builder.toString)
      }
      catch { case _: UnknownSig => None }
    }
    else None
  }

  /** Extracts the type of the thrown exception from an AnnotationInfo.
    *
    * Supports both “old-style” `@throws(classOf[Exception])`
    * as well as “new-style” `@throws[Exception]("cause")` annotations.
    */
  object ThrownException {
    def unapply(ann: Annotation)(implicit ctx: Context): Option[Type] = {
      ann.tree match {
        case Apply(TypeApply(fun, List(tpe)), _) if tpe.isType && fun.symbol.owner == defn.ThrowsAnnot && fun.symbol.isConstructor =>
          Some(tpe.typeOpt)
        case _ =>
          None
      }
    }
  }


  class UnknownSig extends Exception

  private def needsJavaSig(tp: Type, throwsArgs: List[Type])(implicit ctx: Context): Boolean = !ctx.settings.YnoGenericSig.value && {
      def needs(tp: Type) = (new NeedsSigCollector).apply(false, tp)
      needs(tp) || throwsArgs.exists(needs)
  }

  // @M #2585 when generating a java generic signature that includes
  // a selection of an inner class p.I, (p = `pre`, I = `cls`) must
  // rewrite to p'.I, where p' refers to the class that directly defines
  // the nested class I.
  //
  // See also #2585 marker in javaSig: there, type arguments must be
  // included (use pre.baseType(cls.owner)).
  //
  // This requires that cls.isClass.
  private def rebindInnerClass(pre: Type, cls: Symbol)(implicit ctx: Context): Type = {
    val owner = cls.owner
    if (owner.is(Flags.PackageClass) || owner.isTerm) pre else cls.owner.info /* .tpe_* */
  }

  object GenericArray {

    /** Is `tp` an unbounded generic type (i.e. which could be instantiated
      *  with primitive as well as class types)?.
      */
    private def genericCore(tp: Type)(implicit ctx: Context): Type = tp.widenDealias match {
      /* A Java Array<T> is erased to Array[Object] (T can only be a reference type), where as a Scala Array[T] is
       * erased to Object. However, there is only symbol for the Array class. So to make the distinction between
       * a Java and a Scala array, we check if the owner of T comes from a Java class.
       * This however caused issue scala/bug#5654. The additional test for EXISTENTIAL fixes it, see the ticket comments.
       * In short, members of an existential type (e.g. `T` in `forSome { type T }`) can have pretty arbitrary
       * owners (e.g. when computing lubs, <root> is used). All packageClass symbols have `isJavaDefined == true`.
       */
      case _: TypeRef =>
        val sym = tp.typeSymbol
        if (sym.isAbstractType && (!sym.owner.is(Flags.JavaDefined) || sym.is(Flags.Scala2Existential)))
          tp
        else
          NoType

      case bounds: TypeBounds =>
        bounds

      case AppliedType(tp, _) =>
        val sym = tp.typeSymbol
        if (sym.isAbstractType && (!sym.owner.is(Flags.JavaDefined) || sym.is(Flags.Scala2Existential)))
          tp
        else
          NoType

      case _ =>
        NoType
    }

    /** If `tp` is of the form Array[...Array[T]...] where `T` is an abstract type
      *  then Some((N, T)) where N is the number of Array constructors enclosing `T`,
      *  otherwise None. Existentials on any level are ignored.
      */
    def unapply(tp: Type)(implicit ctx: Context): Option[(Int, Type)] = tp.widenDealias match {
      case AppliedType(tp, arg :: Nil) =>
        val test = tp.typeSymbol == defn.ArrayClass
        if (!test) return None
        genericCore(arg) match {
          case NoType =>
            unapply(arg) match {
              case Some((level, core)) => Some((level + 1, core))
              case None => None
            }
          case core =>
            Some((1, core))
        }
      case _ =>
        None
    }

  }

  private object RefOrAppliedType {
    def unapply(tp: Type)(implicit ctx: Context): Option[(Symbol, Type, List[Type])] = tp match {
      case TypeParamRef(_, _) =>
        Some((tp.typeSymbol, tp, Nil))
      case TermParamRef(_, _) =>
        Some((tp.termSymbol, tp, Nil))
      case TypeRef(pre, _) if !tp.typeSymbol.isAliasType =>
        val sym = tp.typeSymbol
        Some((sym, pre, Nil))
      case AppliedType(pre, args) =>
        Some((pre.typeSymbol, pre, args))
      case _ =>
        None
    }
  }


  private class NeedsSigCollector(implicit ctx: Context) extends TypeAccumulator[Boolean] {
    override def apply(x: Boolean, tp: Type): Boolean =
      if (!x) {
        tp match {
          case RefinedType(parent, refinedName, refinedInfo) =>
            val sym = parent.typeSymbol
            if (sym == defn.ArrayClass) foldOver(x, refinedInfo)
            else true
          case tref @ TypeRef(pre, name) =>
            val sym = tref.typeSymbol
            if (sym.is(Flags.TypeParam) || sym.typeParams.nonEmpty) true
            else if (sym.isClass) foldOver(x, rebindInnerClass(pre, sym)) // #2585
            else foldOver(x, pre)
          case PolyType(_, _) =>
            true
          case ClassInfo(_, _, parents, _, _) =>
            foldOver(tp.typeParams.nonEmpty, parents)
          case AnnotatedType(tpe, _) =>
            foldOver(x, tpe)
          case proxy: TypeProxy =>
            foldOver(x, proxy)
          case _ =>
            foldOver(x, tp)
        }
      } else x
  }
}
