package dotty.tools.scaladoc.tasty

import collection.JavaConverters._
import dotty.tools.scaladoc._
import dotty.tools.scaladoc.{Signature => DSignature}
import dotty.tools.scaladoc.Inkuire

import scala.util.Random
import scala.quoted._

import SymOps._
import NameNormalizer._
import SyntheticsSupport._

trait InkuireSupport:
  self: TastyParser =>
  import qctx.reflect._

  private given qctx.type = qctx

  private def paramsForClass(classDef: ClassDef, vars: Set[String]): Seq[Inkuire.Variance] =
    classDef.getTypeParams.map(mkTypeArgumentInkuire)

  given TreeSyntaxInkuire: AnyRef with
    extension (tpeTree: Tree)
      def asInkuire(vars: Set[String]): Inkuire.TypeLike =
        partialAsInkuire(vars)(tpeTree)

  def partialAsInkuire(vars: Set[String]): PartialFunction[Tree, Inkuire.TypeLike] = {
    case TypeBoundsTree(low, high) => inner(low.tpe, vars) //TODO [Inkuire] Type bounds
    case tpeTree: Applied =>
      inner(tpeTree.tpe, vars)
    case tpeTree: TypeTree =>
      inner(tpeTree.tpe, vars)
    case term:  Term => inner(term.tpe, vars)
    case classDef: ClassDef => mkTypeFromClassDef(classDef, vars)
    case typeDef: TypeDef =>
      Inkuire.Type(
        name = Inkuire.TypeName(typeDef.name),
        itid = typeDef.symbol.itid
      )
  }

  def mkTypeFromClassDef(classDef: ClassDef, vars: Set[String]): Inkuire.Type = {
    Inkuire.Type(
      name = Inkuire.TypeName(classDef.name),
      itid = classDef.symbol.itid,
      params = paramsForClass(classDef, vars)
    )
  }

  given SymbolSyntaxInkuire: AnyRef with
    extension (symbol: Symbol)
      def itid(using dctx: DocContext): Option[Inkuire.ITID] = Some(Inkuire.ITID(symbol.dri.symbolUUID, isParsed = false))

  given TypeSyntaxInkuire: AnyRef with
    extension (tpe: TypeRepr)
      def asInkuire(vars: Set[String]): Inkuire.TypeLike = inner(tpe, vars)

  private def genSyntheticTypeArgs(n: Int) =
    1.to(n).map { i =>
      val uuid = s"synthetic-arg$i${Random.nextString(10)}"
      val name = s"X$i"
      Inkuire.Type(
        name = Inkuire.TypeName(name),
        itid = Some(Inkuire.ITID(uuid, isParsed = false)),
        isVariable = true
      )
    }

  def mkTypeArgumentInkuire(argument: TypeDef): Inkuire.Variance =
    //TODO [Inkuire] Type bounds (other than just HKTs)
    val name = argument.symbol.normalizedName
    val normalizedName = if name.matches("_\\$\\d*") then "_" else name
    val params = genSyntheticTypeArgs(typeVariableDeclarationParamsNo(argument))
    val res = Inkuire.Type(
      name = Inkuire.TypeName(normalizedName),
      itid = argument.symbol.itid,
      isVariable = true,
      params = params.map(Inkuire.Invariance(_))
    )
    val t = params.toList match
      case Nil => res
      case _ => Inkuire.TypeLambda(params, res)
    if argument.symbol.flags.is(Flags.Covariant) then Inkuire.Covariance(t)
    else if argument.symbol.flags.is(Flags.Contravariant) then Inkuire.Contravariance(t)
    else Inkuire.Invariance(t)

  def typeVariableDeclarationParamsNo(argument: TypeDef): Int =
    argument.rhs match
      case t: TypeTree => t.tpe match
        case TypeBounds(_, TypeLambda(names, _, _)) => names.size
        case _ => 0
      case _ => 0

  private def isRepeatedAnnotation(term: Term) =
    term.tpe match
      case t: TypeRef => t.name == "Repeated" && t.qualifier.match
        case ThisType(tref: TypeRef) if tref.name == "internal" => true
        case _ => false
      case _ => false

  private def isRepeated(typeRepr: TypeRepr) =
    typeRepr match
      case t: TypeRef => t.name == "<repeated>" && t.qualifier.match
        case ThisType(tref: TypeRef) if tref.name == "scala" => true
        case _ => false
      case _ => false

  private def inner(tp: TypeRepr, vars: Set[String]): Inkuire.TypeLike = tp match
    case OrType(left, right) => Inkuire.OrType(inner(left, vars), inner(right, vars))
    case AndType(left, right) => Inkuire.AndType(inner(left, vars), inner(right, vars))
    case ByNameType(tpe) => inner(tpe, vars)
    case ConstantType(constant) =>
      Inkuire.Type(
        name = Inkuire.TypeName(constant.toString),
        params = Seq.empty,
        itid = Some(Inkuire.ITID(constant.toString, isParsed = false))
      )
    case ThisType(tpe) => inner(tpe, vars)
    case AnnotatedType(AppliedType(_, Seq(tpe)), annotation) if isRepeatedAnnotation(annotation) =>
      inner(tpe, vars) //TODO [Inkuire] Repeated types
    case AppliedType(repeatedClass, Seq(tpe)) if isRepeated(repeatedClass) =>
      inner(tpe, vars) //TODO [Inkuire] Repeated types
    case AnnotatedType(tpe, _) =>
      inner(tpe, vars)
    case tl @ TypeLambda(paramNames, _, resType) =>
      Inkuire.TypeLambda(paramNames.map(Inkuire.TypeLambda.argument), inner(resType, vars)) //TODO [Inkuire] Type bounds
    case r: Refinement =>
      inner(r.info, vars) //TODO [Inkuire] Refinements
    case t @ AppliedType(tpe, typeList) =>
      import dotty.tools.dotc.util.Chars._
      if t.isFunctionType then
        val name = s"Function${typeList.size-1}"
        Inkuire.Type(
          name = Inkuire.TypeName(name),
          params = typeList.init.map(p => Inkuire.Contravariance(inner(p, vars))) :+ Inkuire.Covariance(inner(typeList.last, vars)),
          itid = Some(Inkuire.ITID(s"${name}scala.${name}//[]", isParsed = false))
        )
      else if t.isTupleType then
        val name = s"Tuple${typeList.size}"
        Inkuire.Type(
          name = Inkuire.TypeName(name),
          params = typeList.map(p => Inkuire.Covariance(inner(p, vars))),
          itid = Some(Inkuire.ITID(s"${name}scala.${name}//[]", isParsed = false))
        )
      else
        inner(tpe, vars).asInstanceOf[Inkuire.Type].copy(
          params = typeList.map(p => Inkuire.Invariance(inner(p, vars)))
        )
    case tp: TypeRef =>
      Inkuire.Type(
        name = Inkuire.TypeName(tp.name),
        itid = tp.typeSymbol.itid,
        params = Seq.empty,
        isVariable = vars.contains(tp.name)
      )
    case tr @ TermRef(qual, typeName) =>
      inner(qual, vars)
    case TypeBounds(low, hi) =>
      inner(low, vars) //TODO [Inkuire] Type bounds
    case NoPrefix() =>
      Inkuire.Type.unresolved //TODO [Inkuire] <- should be handled by Singleton case, but didn't work
    case MatchType(bond, sc, cases) =>
      inner(sc, vars)
    case ParamRef(TypeLambda(names, _, _), i) =>
      Inkuire.TypeLambda.argument(names(i))
    case ParamRef(m: MethodType, i) =>
      inner(m.paramTypes(i), vars)
    case RecursiveType(tp) =>
      inner(tp, vars)
    case MethodType(_, params, resType) =>
      inner(resType, vars) //TODO [Inkuire] Method type
