package dotty.tools.dotc
package transform

import core._
import Types._
import Contexts._
import Symbols._
import Decorators._
import StdNames.nme
import NameOps._
import language.implicitConversions

object TypeUtils {
  implicit def decorateTypeUtils(tpe: Type): TypeUtils = new TypeUtils(tpe)
}

/** A decorator that provides methods for type transformations
 *  that are needed in the transofmer pipeline
 */
class TypeUtils(val self: Type) extends AnyVal {
  import TypeUtils._

  /** Converts the type of a member of class `clazz` to a method type that
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
  def toStatic(clazz: ClassSymbol)(implicit ctx: Context): Type = {
    val (mtparamCount, origResult) = self match {
      case self @ PolyType(mtnames) => (mtnames.length, self.resultType)
      case self: ExprType => (0, self.resultType)
      case _ => (0, self)
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

    self match {
      case self @ PolyType(mtnames) =>
        PolyType(mtnames ++ ctnames)(
          pt => (self.paramBounds ++ mappedClassBounds(pt))
            .mapConserve(_.subst(self, pt).bounds),
          pt => mappedResultType(pt).subst(self, pt))
      case _ =>
        if (ctparams.isEmpty) resultType
        else PolyType(ctnames)(mappedClassBounds, mappedResultType)
    }
  }

  /** Assuming `self` is a result of a `toStatic` call, the signature of the
   *  original method type `X` such that `self = X.toStatic`.
   */
  def dynamicSignature(implicit ctx: Context): Signature = self match {
    case self: PolyType => self.resultType.dynamicSignature
    case self @ MethodType(nme.SELF :: Nil, _) =>
      val normalizedResultType = self.resultType match {
        case rtp: MethodType => rtp
        case rtp => ExprType(rtp)
      }
      normalizedResultType.signature
    case _ =>
      Signature.NotAMethod
  }

  /** The Seq type corresponding to this repeated parameter type */
  def repeatedToSeq(implicit ctx: Context) =
    self.translateParameterized(defn.RepeatedParamClass, defn.SeqClass)
}