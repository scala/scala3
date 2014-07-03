package dotty.tools.dotc
package transform

import core._
import Types._
import Contexts._
import Symbols._
import Decorators._
import StdNames.nme
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
      case _ => (0, self)
    }
    val ctparams = clazz.typeParams
    val ctnames = ctparams.map(_.name)

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

  /** Converts from the result of a `toStatic(clazz)` back to the original type.
   *
   *  To do this, it removes the `$this` argument from the parameter list a method,
   *  and converts trailing type parameters of the method to the type parameters of
   *  the given `clazz`.
   *
   *  If `stpe` is a `PolyType`, any parameters corresponding to class type parameters
   *  are remapped and `$this` is removed from the result type.
   *  If `stpe` is a `MethodType`, it may have a curried parameter list with the
   *  `$this` alone in the first parameter list, in which case that parameter list
   *  is dropped.  Or, since the curried lists disappear during uncurry, it may have
   *  a single parameter list with `$this` as the first parameter, in which case that
   *  parameter is removed from the list. Note that we do not need to adjust the result
   *  type with substParams because at uncurry there are no more depdendent method types.
   */
  def toDynamic(clazz: Symbol)(implicit ctx: Context): Type = self match {
    case self: PolyType =>
      // contains method type parameters, followed by class type parameters
      val nparams = self.paramNames.length - clazz.typeParams.length
      val (mNames, cNames) = self.paramNames.splitAt(nparams)
      val (mBounds, cBounds) = self.paramBounds.splitAt(nparams)
      val mappedParams =
        (0 until nparams).toList.map(PolyParam(self, _)) ++ clazz.typeParams.map(_.typeRef)
      def mapParams(tp: Type, pt: PolyType) = {
        val mapped = (0 until nparams).toList.map(PolyParam(pt, _)) ++ clazz.typeParams.map(_.typeRef)
        tp.substParams(self, mapped)
      }
      val restpe = self.resultType.toDynamic(clazz).substParams(self, mappedParams)
      if (nparams == 0) mapParams(restpe, self)
      else PolyType(self.paramNames.take(nparams))(
             pt => self.paramBounds.mapconserve(mapParams(_, pt).asInstanceOf[TypeBounds]),
             pt => mapParams(restpe, pt))
    case mt @ MethodType(nme.SELF :: otherNames, thizType :: otherTypes) =>
      val remainder =
        if (otherNames.isEmpty) mt.resultType
        else MethodType(otherNames, otherTypes, mt.resultType)
      remainder.substParam(MethodParam(mt, 0), clazz.thisType)
   case _ =>
      self
  }
}