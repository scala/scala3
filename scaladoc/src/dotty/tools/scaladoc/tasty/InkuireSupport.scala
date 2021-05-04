package dotty.tools.scaladoc.tasty

import collection.JavaConverters._
import dotty.tools.scaladoc._
import dotty.tools.scaladoc.{Signature => DSignature}
import dotty.tools.scaladoc.Inkuire

trait InkuireSupport:
  self: TastyParser =>
  import qctx.reflect._

  private def paramsForClass(classDef: ClassDef, vars: Set[String], isVariable: Boolean): Seq[Inkuire.Variance] =
    classDef.getTypeParams.map(mkTypeArgumentInkuire(_, vars, isVariable))

  given TreeSyntaxInkuire: AnyRef with
    extension (tpeTree: Tree)
      def asInkuire(vars: Set[String], isVariable: Boolean): Inkuire.Type =
        tpeTree match
          case TypeBoundsTree(low, high) => inner(low.tpe, vars) //TODO
          case tpeTree: Applied =>
            inner(tpeTree.tpe, vars).copy(
              params = tpeTree.args.map(p => Inkuire.Invariance(p.asInkuire(vars, isVariable))) //TODO check variance
            )
          case tpeTree: TypeTree => inner(tpeTree.tpe, vars)
          case term:  Term => inner(term.tpe, vars)
          case classDef: ClassDef => mkTypeFromClassDef(classDef, vars, isVariable)

  def mkTypeFromClassDef(classDef: ClassDef, vars: Set[String], isVariable: Boolean): Inkuire.Type = {
    Inkuire.Type(
      name = Inkuire.TypeName(classDef.name),
      itid = classDef.symbol.itid,
      params = paramsForClass(classDef, vars, isVariable)
    )
  }

  given SymbolSyntaxInkuire: AnyRef with
    extension (symbol: Symbol)
      def itid(using dctx: DocContext): Option[Inkuire.ITID] = Some(Inkuire.ITID(symbol.dri.symbolUUID, isParsed = false))
  
  given TypeSyntaxInkuire: AnyRef with
    extension (tpe: TypeRepr)
      def asInkuire(vars: Set[String]): Inkuire.Type = inner(tpe, vars)

  def mkTypeArgumentInkuire(argument: TypeDef, vars: Set[String] = Set.empty, isVariable: Boolean = false): Inkuire.Variance =
    val name = argument.symbol.normalizedName
    val normalizedName = if name.matches("_\\$\\d*") then "_" else name
    val t = Inkuire.Type(
      name = Inkuire.TypeName(normalizedName),
      itid = argument.symbol.itid,
      isVariable = vars.contains(normalizedName) || isVariable,
      params = Seq.empty //TODO in future arities of params will be needed
    )
    if argument.symbol.flags.is(Flags.Covariant) then Inkuire.Covariance(t)
    else if argument.symbol.flags.is(Flags.Contravariant) then Inkuire.Contravariance(t)
    else Inkuire.Invariance(t)

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

  private def inner(tp: TypeRepr, vars: Set[String]): Inkuire.Type = tp match
    case OrType(left, right) => inner(left, vars) //TODO for future
    case AndType(left, right) => inner(left, vars) //TODO for future
    case ByNameType(tpe) => inner(tpe, vars)
    case ConstantType(constant) =>
      ??? //TODO for future, kinda
    case ThisType(tpe) => inner(tpe, vars)
    case AnnotatedType(AppliedType(_, Seq(tpe)), annotation) if isRepeatedAnnotation(annotation) =>
      inner(tpe, vars) //TODO for future
    case AppliedType(repeatedClass, Seq(tpe)) if isRepeated(repeatedClass) =>
      inner(tpe, vars) //TODO for future
    case AnnotatedType(tpe, _) =>
      inner(tpe, vars)
    case tl @ TypeLambda(params, paramBounds, resType) =>
      if resType.typeSymbol.name == "Seq" then println(resType)
      inner(resType, vars) //TODO for future
    case r: Refinement =>
      inner(r.info, vars) //TODO for future
    case t @ AppliedType(tpe, typeList) =>
      import dotty.tools.dotc.util.Chars._
      if !t.typeSymbol.name.forall(isIdentifierPart) && typeList.size == 2 then
        inner(typeList.head, vars)
      else if t.isFunctionType then
        typeList match
          case Nil =>
            ??? //Not possible right?
          case args =>
            val name = s"Function${args.size-1}"
            Inkuire.Type(
              name = Inkuire.TypeName(name),
              params = args.init.map(p => Inkuire.Contravariance(inner(p, vars))) :+ Inkuire.Covariance(inner(args.last, vars)),
              itid = Some(Inkuire.ITID(s"${name}scala.${name}//[]", isParsed = false))
            )
      else if t.isTupleType then
        typeList match
          case Nil =>
            ??? //TODO Not possible right?
          case args =>
            val name = s"Tuple${args.size}"
            Inkuire.Type(
              name = Inkuire.TypeName(name),
              params = args.map(p => Inkuire.Covariance(inner(p, vars))),
              itid = Some(Inkuire.ITID(s"${name}scala.${name}//[]", isParsed = false))
            )
      else
        inner(tpe, vars).copy(
          params = typeList.map(p => Inkuire.Invariance(inner(p, vars)))
        ) //TODO check if it's ok (Having resolver should mean that variance here isn't meaningful)
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
      inner(low, vars) //TODO for future
    case NoPrefix() =>
      ??? //TODO not possible right?
    case MatchType(bond, sc, cases) =>
      inner(sc, vars)
    case ParamRef(TypeLambda(names, _, resType), i) =>
      Inkuire.Type(
        name = Inkuire.TypeName(names(i)),
        itid = Some(Inkuire.ITID(s"external-itid-${names(i)}", isParsed = false)), //TODO check if it's possible to get the actual ITID(DRI)
        isVariable = true
      )
    case ParamRef(m: MethodType, i) =>
      inner(m.paramTypes(i), vars)
    case RecursiveType(tp) =>
      inner(tp, vars)