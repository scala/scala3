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
import core.Decorators._
import core.Constants._
import typer.NoChecking
import typer.ProtoTypes._
import typer.ErrorReporting._
import core.transform.Erasure._
import core.Decorators._
import ast.{tpd, untpd}
import ast.Trees._

class Erasure extends Phase with DenotTransformer {

  override def name: String = "erasure"

  def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = ref match {
    case ref: SymDenotation =>
      assert(ctx.phase == this, s"transforming $ref at ${ctx.phase}")
      ref.copySymDenotation(info = transformInfo(ref.symbol, ref.info))
    case ref =>
      ref.derivedSingleDenotation(ref.symbol, erasure(ref.info))
  }

  val eraser = new Erasure.Typer

  def run(implicit ctx: Context): Unit = {
    val unit = ctx.compilationUnit
    unit.tpdTree = eraser.typedExpr(unit.tpdTree)(ctx.fresh.setPhase(this.next))
  }
}

object Erasure {

  import tpd._

  object Boxing {

    def isUnbox(sym: Symbol)(implicit ctx: Context) =
      sym.name == nme.unbox && (defn.ScalaBoxedClasses contains sym.owner)

    def isBox(sym: Symbol)(implicit ctx: Context) =
      sym.name == nme.box && (defn.ScalaValueClasses contains sym.owner)

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
      if isUnbox(fn.symbol) && (defn.ScalaBoxedClasses contains arg.tpe.widen.typeSymbol) =>
        arg
      case _ =>
        EmptyTree
    }

    def isErasedValueType(tpe: Type)(implicit ctx: Context): Boolean = tpe.isInstanceOf[ErasedValueType]
    def isPrimitiveValueType(tpe: Type)(implicit ctx: Context): Boolean = tpe.classSymbol.isPrimitiveValueClass

    def constant(tree: Tree, const: Tree)(implicit ctx: Context) =
      if (isIdempotentExpr(tree)) Block(tree :: Nil, const) else const

    final def box(tree: Tree, target: => String = "")(implicit ctx: Context): Tree = ctx.traceIndented(i"boxing ${tree.showSummary}: ${tree.tpe} into $target") {
      tree.tpe.widen match {
        case ErasedValueType(clazz, _) =>
          New(clazz.typeRef, cast(tree, clazz.underlyingOfValueClass) :: Nil) // todo: use adaptToType?
        case tp =>
          val cls = tp.classSymbol
          if (cls eq defn.UnitClass) constant(tree, ref(defn.BoxedUnit_UNIT))
          else if (cls eq defn.NothingClass) tree // a non-terminating expression doesn't need boxing
          else {
            assert(cls ne defn.ArrayClass)
            val arg = safelyRemovableUnboxArg(tree)
            if (arg.isEmpty) Apply(ref(boxMethod(cls.asClass)), tree :: Nil)
            else {
              ctx.log(s"boxing an unbox: ${tree.symbol} -> ${arg.tpe}")
              arg
            }
          }
      }
    }

    def unbox(tree: Tree, pt: Type)(implicit ctx: Context): Tree = ctx.traceIndented(i"unboxing ${tree.showSummary}: ${tree.tpe} as a $pt") {
      pt match {
        case ErasedValueType(clazz, underlying) =>
          val tree1 =
            if ((tree.tpe isRef defn.NullClass) && isPrimitiveValueType(underlying))
              // convert `null` directly to underlying type, as going
              // via the unboxed type would yield a NPE (see SI-5866)
              unbox(tree, underlying)
            else
              Apply(Select(adaptToType(tree, clazz.typeRef), clazz.valueClassUnbox), Nil)
          cast(tree1, pt)
        case _ =>
          val cls = pt.classSymbol
          if (cls eq defn.UnitClass) constant(tree, Literal(Constant(())))
          else {
            assert(cls ne defn.ArrayClass)
            Apply(ref(unboxMethod(cls.asClass)), tree :: Nil)
          }
      }
    }

    /** Generate a synthetic cast operation from tree.tpe to pt.
     */
    def cast(tree: Tree, pt: Type)(implicit ctx: Context): Tree =
      if (pt isRef defn.UnitClass) unbox(tree, pt)
      else (tree.tpe, pt) match {
        case (defn.ArrayType(treeElem), defn.ArrayType(ptElem))
        if isPrimitiveValueType(treeElem.widen) && !isPrimitiveValueType(ptElem) =>
          // See SI-2386 for one example of when this might be necessary.
          cast(runtimeCall(nme.toObjectArray, tree :: Nil), pt)
        case _ =>
          ctx.log(s"casting from ${tree.showSummary}: ${tree.tpe.show} to ${pt.show}")
          TypeApply(Select(tree, defn.Object_asInstanceOf), TypeTree(pt) :: Nil)
      }

    /** Adaptation of an expression `e` to an expected type `PT`, applying the following
     *  rewritings exhaustively as long as the type of `e` is not a subtype of `PT`.
     *
     *    e -> box(e)        if `e` is of erased value type
     *    e -> unbox(e, PT)  otherwise, if `PT` is an erased value type
     *    e -> box(e)        if `e` is of primitive type and `PT` is not a primitive type
     *    e -> unbox(e, PT)  if `PT` is a primitive type and `e` is not of primitive type
     *    e -> cast(e, PT)   otherwise
     */
    def adaptToType(tree: Tree, pt: Type)(implicit ctx: Context): Tree =
      if (tree.tpe <:< pt)
        tree
      else if (isErasedValueType(tree.tpe.widen))
        adaptToType(box(tree), pt)
      else if (isErasedValueType(pt))
        adaptToType(unbox(tree, pt), pt)
      else if (isPrimitiveValueType(tree.tpe.widen) && !isPrimitiveValueType(pt))
        adaptToType(box(tree), pt)
      else if (isPrimitiveValueType(pt) && !isPrimitiveValueType(tree.tpe.widen))
        adaptToType(unbox(tree, pt), pt)
      else
        cast(tree, pt)
  }

  class Typer extends typer.Typer with NoChecking {
    import Boxing._

    def erasedType(tree: untpd.Tree)(implicit ctx: Context): Type =
     erasure(tree.tpe.asInstanceOf[Type])

    private def promote(tree: untpd.Tree)(implicit ctx: Context): tree.ThisTree[Type] = {
      assert(tree.hasType)
      val erased = erasedType(tree)(ctx.withPhase(ctx.erasurePhase))
      ctx.log(s"promoting ${tree.show}: ${erased.showWithUnderlying()}")
      tree.withType(erased)
    }

    override def typedIdent(tree: untpd.Ident, pt: Type)(implicit ctx: Context): Tree = {
      val tree1 = promote(tree)
      tree1.tpe match {
        case ThisType(cls) => This(cls) withPos tree.pos
        case _ => tree1
      }
    }

    /** Type check select nodes, applying the following rewritings exhaustively
     *  on selections `e.m`.
     *
     *      e.m1 -> e.m2        if `m1` is a member of Any or AnyVal and `m2` is
     *                          the same-named member in Object.
     *      e.m -> box(e).m     if `e` is primitive and `m` is a member or a reference class
     *                          or `e` has an erased value class type.
     *      e.m -> unbox(e).m   if `e` is not primitive and `m` is a member of a primtive type.
     *
     *  Additionally, if the type of `e` does not derive from the type `OT` of the owner of `m`,
     *  the following rewritings are performed, where `ET` is the erased type of the selection's
     *  original qualifier expression.
     *
     *      e.m -> cast(OT).m   if `m` is not an array operation
     *      e.m -> cast(ET).m   if `m` is an array operation and `ET` is an array type
     *      e.m -> runtime.array_m(e)
     *                          if `m` is an array operation and `ET` is Object
     */
    override def typedSelect(tree: untpd.Select, pt: Type)(implicit ctx: Context): Tree = {
      val sym = tree.symbol
      assert(sym.exists)

      def select(qual: Tree, sym: Symbol): Tree =
        untpd.cpy.Select(tree, qual, sym.name) withType qual.tpe.select(sym)

      def selectArrayMember(qual: Tree, erasedPre: Type) =
        if (erasedPre isRef defn.ObjectClass) runtimeCall(tree.name.genericArrayOp, qual :: Nil)
        else recur(cast(qual, erasedPre))

      def recur(qual: Tree): Tree = {
        val qualIsPrimitive = isPrimitiveValueType(qual.tpe)
        val symIsPrimitive = sym.owner.isPrimitiveValueClass
        if ((sym.owner eq defn.AnyClass) || (sym.owner eq defn.AnyValClass))
          select(qual, defn.ObjectClass.info.decl(sym.name).symbol)
        else if (qualIsPrimitive && !symIsPrimitive || isErasedValueType(qual.tpe))
          recur(box(qual))
        else if (!qualIsPrimitive && symIsPrimitive)
          recur(unbox(qual, sym.owner.typeRef))
        else if (qual.tpe.derivesFrom(sym.owner) || qual.isInstanceOf[Super])
          select(qual, sym)
        else if (sym.owner eq defn.ArrayClass)
          selectArrayMember(qual, erasure(tree.qualifier.tpe))
        else
          recur(cast(qual, sym.owner.typeRef))
      }

      recur(typed(tree.qualifier, AnySelectionProto))
    }

    override def typedTypeApply(tree: untpd.TypeApply, pt: Type)(implicit ctx: Context) = {
      val TypeApply(fun, args) = tree
      val fun1 = typedExpr(fun, pt)
      fun1.tpe.widen match {
        case funTpe: PolyType =>
          val args1 = args.mapconserve(typedType(_))
          untpd.cpy.TypeApply(tree, fun1, args1).withType(funTpe.instantiate(args1.tpes))
        case _ => fun1
      }
    }

    override def typedApply(tree: untpd.Apply, pt: Type)(implicit ctx: Context): Tree = {
      val Apply(fun, args) = tree
      val fun1 = typedExpr(fun, WildcardType)
      fun1.tpe.widen match {
        case mt: MethodType =>
          val args1 = args.zipWithConserve(mt.paramTypes)(typedExpr)
          untpd.cpy.Apply(tree, fun1, args1) withType mt.resultType
      }
    }

    override def typedTypeTree(tree: untpd.TypeTree, pt: Type)(implicit ctx: Context): TypeTree =
      promote(tree)

    override def ensureNoLocalRefs(block: Block, pt: Type, forcedDefined: Boolean = false)(implicit ctx: Context): Tree =
      block // optimization, no checking needed, as block symbols do not change.

    override def typedDefDef(ddef: untpd.DefDef, sym: Symbol)(implicit ctx: Context) = {
      val tpt1 = // keep UnitTypes intact in result position
        if (ddef.tpt.typeOpt isRef defn.UnitClass) untpd.TypeTree(defn.UnitType) withPos ddef.tpt.pos
        else ddef.tpt
      val ddef1 = untpd.cpy.DefDef(ddef, ddef.mods, ddef.name, Nil, ddef.vparamss, tpt1, ddef.rhs)
      super.typedDefDef(ddef1, sym)
    }

    override def typedClassDef(cdef: untpd.TypeDef, sym: ClassSymbol)(implicit ctx: Context) = {
      val TypeDef(mods, name, impl @ Template(constr, parents, self, body)) = cdef
      val cdef1 = untpd.cpy.TypeDef(cdef, mods, name,
        untpd.cpy.Template(impl, constr, parents, untpd.EmptyValDef, body))
      super.typedClassDef(cdef1, sym)
    }

     /*
    override def transformStats(stats: List[Tree], exprOwner: Symbol)(implicit ctx: Context) = {
      val stats1 = super.transform(stats, exprOwner)
      if (ctx.owner.isClass) addBridges(stats1) else stats1
    }
*/
    override def typedNamed(tree: untpd.NameTree, pt: Type)(implicit ctx: Context): Tree = {
      if (tree eq untpd.EmptyValDef) return tpd.EmptyValDef
      assert(tree.hasType, tree.show)
      val sym = tree.symbol
      def localContext = ctx.fresh.setTree(tree).setOwner(sym)
      tree match {
        case tree: untpd.Ident => typedIdent(tree, pt)
        case tree: untpd.Select => typedSelect(tree, pt)
        case tree: untpd.ValDef => typedValDef(tree, sym)(localContext)
        case tree: untpd.DefDef => typedDefDef(tree, sym)(localContext)
        case tree: untpd.TypeDef =>
          if (tree.isClassDef) typedClassDef(tree, sym.asClass)(localContext)
          else EmptyTree
      }
    }

    override def adapt(tree: Tree, pt: Type)(implicit ctx: Context): Tree =
      ctx.traceIndented(i"adapting ${tree.showSummary}: ${tree.tpe} to $pt", show = true) {
        assert(ctx.phase == ctx.erasurePhase.next, ctx.phase)
        if (tree.isEmpty) tree else adaptToType(tree, pt)
    }

    override def index(trees: List[untpd.Tree])(implicit ctx: Context) = ctx
  }
}