package dotty.tools.scaladoc.tasty

import collection.JavaConverters._
import dotty.tools.scaladoc._
import dotty.tools.scaladoc.{Signature => DSignature}
import dotty.tools.scaladoc.Inkuire
import dotty.tools.scaladoc.renderers.Resources

import scala.util.Random
import scala.quoted._
import scala.util.chaining._

import SymOps._
import NameNormalizer._
import SyntheticsSupport._

trait InkuireSupport(using DocContext) extends Resources:
  self: TastyParser =>
  import qctx.reflect._

  // Unused in InkuireSupport, required for Resources
  override def effectiveMembers: Map[DRI, Member] = Map.empty

  private given qctx.type = qctx

  def doInkuireStuff(classDef: ClassDef): Unit = {
    val classType: Inkuire.Type = classDef.asInkuire(Set.empty).asInstanceOf[Inkuire.Type]

    val variableNames: Set[String] = classType.params.map(_.typ)
      .flatMap(varName(_).toList).toSet

    val parents: Seq[Inkuire.Type] = classDef.parents.map(_.asInkuire(variableNames).asInstanceOf[Inkuire.Type])

    val isModule = classDef.symbol.flags.is(Flags.Module)

    if !isModule then Inkuire.db = Inkuire.db.copy(types = Inkuire.db.types.updated(classType.itid.get, (classType, parents)))

    classDef.symbol.declaredTypes
      .filter(viableSymbol)
      .foreach {
        case typeSymbol: Symbol if typeSymbol.flags.is(Flags.Opaque) =>
          val typ = typeSymbol.tree.asInkuire(variableNames)
          if typ.isInstanceOf[Inkuire.Type] then {
            val t = typ.asInstanceOf[Inkuire.Type]
            Inkuire.db = Inkuire.db.copy(types = Inkuire.db.types.updated(t.itid.get, (t, Seq.empty)))
          }
        case typeSymbol: Symbol if !typeSymbol.isClassDef =>
          val typeDef = typeSymbol.tree.asInstanceOf[TypeDef]
          val typ = typeSymbol.tree.asInkuire(variableNames)
          if typ.isInstanceOf[Inkuire.Type] then {
            val t = typ.asInstanceOf[Inkuire.Type]
            val rhsTypeLike = typeDef.rhs.asInkuire(variableNames)
            val withType = Inkuire.db.copy(
              types = Inkuire.db.types.updated(t.itid.get, (t, Seq.empty))
            )
            typeDef.rhs match {
              case TypeBoundsTree(_, r) if r.asInkuire(variableNames).isInstanceOf[Inkuire.Type] =>
                Inkuire.db = Inkuire.db.copy(
                  types = Inkuire.db.types.updated(t.itid.get, (t, Seq(r.asInkuire(variableNames).asInstanceOf[Inkuire.Type])))
                )
              case _ =>
                Inkuire.db = withType.copy(
                  types = Inkuire.db.types.updated(t.itid.get, (t, Seq.empty)),
                  typeAliases = Inkuire.db.typeAliases.updated(t.itid.get, rhsTypeLike)
                )
            }
          }
          if typeDef.rhs.symbol.flags.is(Flags.JavaDefined) then
            val typJava = typeDef.rhs.asInkuire(variableNames)
            if typJava.isInstanceOf[Inkuire.Type] then {
              val tJava = typJava.asInstanceOf[Inkuire.Type]
              Inkuire.db = Inkuire.db.copy(types = Inkuire.db.types.updated(tJava.itid.get, (tJava, Seq.empty)))
            }
        case _ =>
    }

    if classDef.symbol.isImplicitClass then // Implicit classes <- synthetic method with the same name
      classDef.symbol.maybeOwner.declarations
        .filter { methodSymbol =>
          methodSymbol.name == classDef.symbol.name && methodSymbol.flags.is(Flags.Implicit) && methodSymbol.flags.is(Flags.Method)
        }
        .foreach(handleImplicitConversion(_, variableNames))

    classDef.symbol.declaredMethods
      .filter(viableSymbol)
      .tap { _.foreach { // Loop for implicit conversions
        case implicitConversion: Symbol if implicitConversion.flags.is(Flags.Implicit) || implicitConversion.flags.is(Flags.Given) =>
          handleImplicitConversion(implicitConversion, variableNames)
        case _ =>
      }}
      .tap { _.foreach { // Loop for functions and vals
        case methodSymbol: Symbol =>
          val defdef = methodSymbol.tree.asInstanceOf[DefDef]
          val methodVars = defdef.paramss.flatMap(_.params).collect {
            case TypeDef(name, _) => name
          }
          val vars = variableNames ++ methodVars
          val (receiver, preArgs): (Option[Inkuire.TypeLike], Seq[Inkuire.TypeLike]) = Some(classType).filter(_ => !isModule) match {
            case None => (methodSymbol.extendedSymbol.flatMap(s => partialAsInkuire(vars).lift(s.tpt)), Seq.empty)
            case rcvr => (rcvr, methodSymbol.extendedSymbol.flatMap(s => partialAsInkuire(vars).lift(s.tpt)).toSeq)
          }
          val (name, ownerName) = nameAndOwnerName(classDef, methodSymbol)
          val sgn = Inkuire.ExternalSignature(
            signature = Inkuire.Signature(
              receiver = receiver,
              arguments = preArgs ++ methodSymbol.nonExtensionTermParamLists.collect {
                case tpc@TermParamClause(params) if !tpc.isImplicit && !tpc.isGiven => params //TODO [Inkuire] Implicit parameters
              }.flatten.map(_.tpt.asInkuire(vars)),
              result = defdef.returnTpt.asInkuire(vars),
              context = Inkuire.SignatureContext(
                vars = vars.toSet,
                constraints = Map.empty //TODO [Inkuire] Type bounds
              )
            ),
            name = name,
            packageName = ownerName,
            uri = methodSymbol.dri.externalLink.getOrElse(absolutePathWithAnchor(methodSymbol.dri)),
            entryType = "def"
          )
          val curriedSgn = sgn.copy(signature = Inkuire.curry(sgn.signature))
          Inkuire.db = Inkuire.db.copy(functions = Inkuire.db.functions :+ curriedSgn)
    }}

    classDef.symbol.declaredFields
      .filter(viableSymbol)
      .foreach {
        case valSymbol: Symbol =>
          val valdef = valSymbol.tree.asInstanceOf[ValDef]
          val receiver: Option[Inkuire.TypeLike] =
            Some(classType)
              .filter(_ => !isModule)
          val (name, ownerName) = nameAndOwnerName(classDef, valSymbol)
          val sgn = Inkuire.ExternalSignature(
            signature = Inkuire.Signature(
              receiver = receiver,
              arguments = Seq.empty,
              result = valdef.tpt.asInkuire(variableNames),
              context = Inkuire.SignatureContext(
                vars = variableNames.toSet,
                constraints = Map.empty //TODO [Inkuire] Type bounds
              )
            ),
            name = name,
            packageName = ownerName,
            uri = valSymbol.dri.externalLink.getOrElse(absolutePathWithAnchor(valSymbol.dri)),
            entryType = "val"
          )
          val curriedSgn = sgn.copy(signature = Inkuire.curry(sgn.signature))
          Inkuire.db = Inkuire.db.copy(functions = Inkuire.db.functions :+ curriedSgn)
      }
  }

  private def handleImplicitConversion(implicitConversion: Symbol, variableNames: Set[String]) = {
    val defdef = implicitConversion.tree.asInstanceOf[DefDef]
    val methodVars = defdef.paramss.flatMap(_.params).collect {
      case TypeDef(name, _) => name
    }
    val vars = variableNames ++ methodVars
    val to = defdef.returnTpt.asInkuire(vars)
    val from = defdef.paramss.flatMap(_.params).collectFirst {
      case v: ValDef => v.tpt.asInkuire(vars)
    }
    (from, to) match
      case (from, to: Inkuire.Type) => Inkuire.implicitConversions = Inkuire.implicitConversions :+ (from -> to)
      case _ =>
  }

  private def nameAndOwnerName(classDef: ClassDef, symbol: Symbol): (String, String) =
    if classDef.symbol.flags.is(Flags.Module) || Seq("apply", "unapply").contains(symbol.name) then
      symbol.maybeOwner.normalizedName + "." + symbol.name ->
        ownerNameChain(classDef.symbol.maybeOwner).mkString(".")
    else
      symbol.name ->
        ownerNameChain(classDef.symbol).mkString(".")

  private def ownerNameChain(sym: Symbol): List[String] =
    if sym.isNoSymbol then List.empty
    else if sym == defn.EmptyPackageClass then List.empty
    else if sym == defn.RootPackage then List.empty
    else if sym == defn.RootClass then List.empty
    else if sym.normalizedName.contains("$package") then ownerNameChain(sym.owner)
    else ownerNameChain(sym.owner) :+ sym.normalizedName

  private def viableSymbol(s: Symbol): Boolean =
      !s.flags.is(Flags.Private) &&
        !s.flags.is(Flags.Protected) &&
        !s.flags.is(Flags.Override) &&
        !s.flags.is(Flags.Synthetic)

  private def varName(t: Inkuire.TypeLike): Option[String] = t match {
    case tpe: Inkuire.Type      => Some(tpe.name.name)
    case tl: Inkuire.TypeLambda => varName(tl.result)
    case _                      => None
  }

  private def paramsForClass(classDef: ClassDef, vars: Set[String]): Seq[Inkuire.Variance] =
    classDef.getTypeParams.map(mkTypeArgumentInkuire)

  given TreeSyntaxInkuire: AnyRef with
    extension (tpeTree: Tree)
      def asInkuire(vars: Set[String]): Inkuire.TypeLike =
        partialAsInkuire(vars)(tpeTree)

  private def partialAsInkuire(vars: Set[String]): PartialFunction[Tree, Inkuire.TypeLike] = {
    case tpeTree: TypeBoundsTree => inner(tpeTree.tpe, vars)
    case tpeTree: Applied =>
      inner(tpeTree.tpe, vars)
    case tpeTree: TypeTree =>
      inner(tpeTree.tpe, vars)
    case term:  Term => inner(term.tpe, vars)
    case classDef: ClassDef => mkTypeFromClassDef(classDef, vars)
    case typeDef: TypeDef => mkTypeDef(typeDef)
  }

  private def mkTypeDef(typeDef: TypeDef): Inkuire.Type = typeDef.rhs match {
    case LambdaTypeTree(paramsDefs, _) =>
      val name = typeDef.symbol.normalizedName
      val normalizedName = if name.matches("_\\$\\d*") then "_" else name
      val params = paramsDefs.map(_.name).map(Inkuire.TypeLambda.argument)
      Inkuire.Type(
        name = Inkuire.TypeName(normalizedName),
        itid = typeDef.symbol.itid,
        params = params.map(Inkuire.Invariance(_))
      )
    case _ =>
      Inkuire.Type(
        name = Inkuire.TypeName(typeDef.name),
        itid = typeDef.symbol.itid
      )
  }

  private def mkTypeFromClassDef(classDef: ClassDef, vars: Set[String]): Inkuire.Type = {
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

  private def genSyntheticTypeArgs(n: Int, resSymbol: Symbol) =
    1.to(n).map { i =>
      val uuid = s"synthetic-arg$i${resSymbol.hashCode}"
      val name = s"X$i"
      Inkuire.Type(
        name = Inkuire.TypeName(name),
        itid = Some(Inkuire.ITID(uuid, isParsed = false)),
        isVariable = true
      )
    }

  private def mkTypeArgumentInkuire(argument: TypeDef): Inkuire.Variance =
    //TODO [Inkuire] Type bounds (other than just HKTs)
    val name = argument.symbol.normalizedName
    val normalizedName = if name.matches("_\\$\\d*") then "_" else name
    val params = genSyntheticTypeArgs(typeVariableDeclarationParamsNo(argument), argument.symbol)
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

  private def typeVariableDeclarationParamsNo(argument: TypeDef): Int =
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

  private def inner(tp: TypeRepr, vars: Set[String]): Inkuire.TypeLike =
    tp match
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
        else if t.isTupleN then
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
      case tb@TypeBounds(low, hi) =>
        if low.typeSymbol != defn.NothingClass || hi.typeSymbol == defn.AnyClass then
          inner(low, vars) //TODO [Inkuire] Type bounds
        else
          inner(hi, vars)
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
      case m@MethodType(_, typeList, resType) =>
        val name = s"Function${typeList.size-1}"
        Inkuire.Type(
          name = Inkuire.TypeName(name),
          params = typeList.map(p => Inkuire.Contravariance(inner(p, vars))) :+ Inkuire.Covariance(inner(resType, vars)),
          itid = Some(Inkuire.ITID(s"${name}scala.${name}//[]", isParsed = false))
        )
