package dotty.tools.dotc
package transform

import core._
import Types._
import Contexts._
import Symbols._
import Decorators._
import StdNames.nme
import NameOps._
import ast._
import ast.Trees._

/** Provides methods to produce fully parameterized versions of instance methods,
 *  where the `this` of the enclosing class is abstracted out in an extra leading
 *  `$this` parameter and type parameters of the class become additional type
 *  parameters of the fully parameterized method.
 *
 *  Example usage scenarios are:
 *
 *    - extension methods of value classes
 *    - implementations of trait methods
 *    - static protected accessors
 *    - local methods produced by tailrec transform
 */
trait FullParameterization {

  import tpd._

  /** If references to original `target` from fully parameterized method `derived` should be
   *  rewired to some fully parameterized method, that method symbol,
   *  otherwise NoSymbol.
   */
  protected def rewiredTarget(target: Symbol, derived: Symbol)(implicit ctx: Context): Symbol

  /** Converts the type `info` of a member of class `clazz` to a method type that
   *  takes the `this` of the class and any type parameters of the class
   *  as additional parameters. Example:
   *
   *    class Foo[+A <: AnyRef](val xs: List[A]) extends AnyVal {
   *      def baz[B >: A](x: B): List[B] = ...
   *    }
   *
   *  leads to:
   *
   *    object Foo {
   *      def extension$baz[B >: A <: Any, A >: Nothing <: AnyRef]($this: Foo[A])(x: B): List[B]
   *    }
   */
  def fullyParameterizedType(info: Type, clazz: ClassSymbol)(implicit ctx: Context): Type = {
    val (mtparamCount, origResult) = info match {
      case info @ PolyType(mtnames) => (mtnames.length, info.resultType)
      case info: ExprType => (0, info.resultType)
      case _ => (0, info)
    }
    val ctparams = clazz.typeParams
    val ctnames = ctparams.map(_.name.unexpandedName())

    /** The method result type, prior to mapping any type parameters */
    val resultType = {
      val thisParamType = clazz.typeRef.appliedTo(ctparams.map(_.typeRef))
      MethodType(nme.SELF :: Nil, thisParamType :: Nil)(mt =>
        origResult.substThis(clazz, MethodParam(mt, 0)))
    }

    /** Replace class type parameters by the added type parameters of the polytype `pt` */
    def mapClassParams(tp: Type, pt: PolyType): Type = {
      val classParamsRange = (mtparamCount until mtparamCount + ctparams.length).toList
      tp.subst(clazz.typeParams, classParamsRange map (PolyParam(pt, _)))
    }

    /** The bounds for the added type paraneters of the polytype `pt` */
    def mappedClassBounds(pt: PolyType): List[TypeBounds] =
      ctparams.map(tparam => mapClassParams(tparam.info, pt).bounds)

    def mappedResultType(pt: PolyType): Type = mapClassParams(resultType, pt)

    info match {
      case info @ PolyType(mtnames) =>
        PolyType(mtnames ++ ctnames)(
          pt => (info.paramBounds ++ mappedClassBounds(pt))
            .mapConserve(_.subst(info, pt).bounds),
          pt => mappedResultType(pt).subst(info, pt))
      case _ =>
        if (ctparams.isEmpty) resultType
        else PolyType(ctnames)(mappedClassBounds, mappedResultType)
    }
  }

  /** Assuming `info` is a result of a `fullyParameterizedType` call, the signature of the
   *  original method type `X` such that `info = fullyParameterizedType(X, ...)`.
   */
  def memberSignature(info: Type)(implicit ctx: Context): Signature = info match {
    case info: PolyType => memberSignature(info.resultType)
    case info @ MethodType(nme.SELF :: Nil, _) =>
      val normalizedResultType = info.resultType match {
        case rtp: MethodType => rtp
        case rtp => ExprType(rtp)
      }
      normalizedResultType.signature
    case _ =>
      Signature.NotAMethod
  }

  /** The type parameters (skolems) of the method definition `originalDef`,
   *  followed by the class parameters of its enclosing class.
   */
  private def allInstanceTypeParams(originalDef: DefDef)(implicit ctx: Context): List[Symbol] =
    originalDef.tparams.map(_.symbol) ::: originalDef.symbol.owner.typeParams

  /** Given an instance method definition `originalDef`, return a
   *  fully parameterized method definition derived from `originalDef`, which
   *  has`derived` as symbol and  `fullyParameterizedType(originalDef.symbol.info)`
   *  as info.
   */
  def fullyParameterizedDef(derived: TermSymbol, originalDef: DefDef)(implicit ctx: Context): Tree = {
    val origMeth = originalDef.symbol
    val origClass = origMeth.owner.asClass
    val origTParams = allInstanceTypeParams(originalDef)
    val origVParams = originalDef.vparamss.flatten map (_.symbol)
    polyDefDef(derived, trefs => vrefss => {
      val thisRef :: argRefs = vrefss.flatten

      /** If tree should be rewired, the rewired tree, otherwise EmptyTree.
       *  @param   targs  Any type arguments passed to the rewired tree.
       */
      def rewire(tree: Tree, targs: List[Tree])(implicit ctx: Context): Tree = {
        def rewireCall(thisArg: Tree): Tree = {
          val sym = tree.symbol
          val rewired = rewiredTarget(sym, derived)
          if (rewired.exists) {
            val base = thisArg.tpe.baseTypeWithArgs(origClass)
            assert(base.exists)
            ref(rewired.termRef)
              .appliedToTypeTrees(targs ++ base.argInfos.map(TypeTree(_)))
              .appliedTo(thisArg)
          } else EmptyTree
        }
        tree match {
          case Ident(_) => rewireCall(thisRef)
          case Select(qual, _) => rewireCall(qual)
          case tree @ TypeApply(fn, targs1) =>
            assert(targs.isEmpty)
            rewire(fn, targs1)
          case Block(stats, expr) =>
            // need special casing here, because an original termRef might leak out as
            // the result of the Block if Block is copied using `cpy`.
            val expr1 = rewire(expr, targs)
            if (expr1.isEmpty) EmptyTree else Block(stats, expr1) withPos tree.pos
          case _ => EmptyTree
        }
      }

      new TreeTypeMap(
        typeMap = _
          .subst(origTParams, trefs)
          .subst(origVParams, argRefs.map(_.tpe))
          .substThis(origClass, thisRef.tpe),
        ownerMap = (sym => if (sym eq origMeth) derived else sym),
        treeMap = {
          case tree: This if tree.symbol == origClass => thisRef
          case tree => rewire(tree, Nil) orElse tree
        }).transform(originalDef.rhs)
    })
  }

  /** A forwarder expression which calls `derived`, passing along
   *  - the type parameters and enclosing class parameters of `originalDef`,
   *  - the `this` of the enclosing class,
   *  - the value parameters of the original method `originalDef`.
   */
  def forwarder(derived: TermSymbol, originalDef: DefDef)(implicit ctx: Context): Tree =
    ref(derived.termRef)
      .appliedToTypes(allInstanceTypeParams(originalDef).map(_.typeRef))
      .appliedTo(This(originalDef.symbol.owner.asClass))
      .appliedToArgss(originalDef.vparamss.nestedMap(vparam => ref(vparam.symbol)))
      .withPos(originalDef.rhs.pos)
}