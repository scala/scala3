package dotty.tools.scaladoc
package tasty

import scala.quoted._
import dotty.tools.scaladoc.util.Escape._
import scala.util.matching.Regex

object Scaladoc2AnchorCreator:

  def getScaladoc2Type(using Quotes)(sym: quotes.reflect.Symbol) = signatureAnchor(sym)

  /** Creates the signature anchor
   *
   *  - `X` for a `type X ...`
   *  - `x:X` for a `val x: X`
   *  - `f[U1,...](x1:T1,...)(impliciti1:U1,impliciti2:U2,...)...:R` for a `def f[U1, ...](x1: T1, ...)(implicit i1: U1, i2: U2...)...: R`
   *
   *  Types are printed without their paths. No spaces are printed in the output.
   */
  private def signatureAnchor(using Quotes)(sym: quotes.reflect.Symbol): String =
    import quotes.reflect.*
    def signatureType(tp: quotes.reflect.TypeRepr): String =
      tp match
        case mt @ MethodType(paramNames, paramTypes, res) =>
          val implicitPrefix = if mt.isImplicit then "implicit" else ""
          val closeClause = res match
            case _: MethodOrPoly => ")"
            case _ => "):"
          paramNames.zip(paramTypes.map(signatureType))
            .map((name, tpe) => s"$implicitPrefix$name:$tpe")
            .mkString("(", ",", closeClause) + signatureType(res)
        case PolyType(paramNames, paramBounds, res) =>
          val closeClause = res match
            case _: MethodOrPoly => "]"
            case _ => "]:"
          paramNames.zip(paramBounds.map(signatureType))
            .map((name, tpe) => s"$name$tpe")
            .mkString("[", ",", closeClause) + signatureType(res)
        case TypeLambda(paramNames, paramBounds, res) =>
          paramNames.zip(paramBounds.map(signatureType))
            .map((name, tpe) => s"$name$tpe")
            .mkString("[", ",", "]") + "=>" + signatureType(res)
        case ByNameType(tp) =>
          ":" + signatureType(tp)
        case TypeBounds(low, hi) =>
          val lowBound = if low =:= defn.NothingClass.typeRef then "" else ">:" + signatureType(low)
          val hiBound = if low =:= defn.AnyClass.typeRef then "" else "<:" + signatureType(hi)
          lowBound + hiBound
        case tp: ParamRef =>
          tp.binder match
            case binder: MethodType => binder.paramNames(tp.paramNum) + ".type"
            case binder: PolyType => binder.paramNames(tp.paramNum)
            case binder: LambdaType => binder.paramNames(tp.paramNum)
        case AppliedType(tycon, args) =>
          args.map {
            case tp: TypeBounds => "_" + signatureType(tp)
            case tp => signatureType(tp)
          }.mkString(signatureType(tycon) + "[", ",", "]")
        case tp: AnnotatedType =>
          signatureType(tp.underlying) + "@" + tp.annotation.symbol.owner.name
        case tp: ThisType =>
          signatureType(tp.tref) + ".this"
        case tp: TypeRef =>
          tp.name
        case tp =>
          // TODO handle other cases without using show (show does not have a stable representation)
          tp.show(using Printer.TypeReprShortCode).replace(" ","")

    sym match
      case sym if sym.isType => sym.name
      case sym if sym.flags.is(Flags.Method) => sym.name + signatureType(sym.info)
      case sym => sym.name + ":" + signatureType(sym.info)
