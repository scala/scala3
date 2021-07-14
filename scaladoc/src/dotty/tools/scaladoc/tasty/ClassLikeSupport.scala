package dotty.tools.scaladoc.tasty

import collection.JavaConverters._
import dotty.tools.scaladoc._
import dotty.tools.scaladoc.{Signature => DSignature}
import dotty.tools.scaladoc.Inkuire

import scala.quoted._

import SymOps._
import NameNormalizer._
import SyntheticsSupport._

trait ClassLikeSupport:
  self: TastyParser =>
  import qctx.reflect._

  private given qctx.type = qctx

  private def bareClasslikeKind(using Quotes)(symbol: reflect.Symbol): Kind =
    import reflect._
    if symbol.flags.is(Flags.Module) then Kind.Object
    else if symbol.flags.is(Flags.Trait) then  Kind.Trait(Nil, Nil)
    else if symbol.flags.is(Flags.Enum) then Kind.Enum(Nil, Nil)
    else if symbol.flags.is(Flags.Enum) && symbol.flags.is(Flags.Case) then Kind.EnumCase(Kind.Object)
    else Kind.Class(Nil, Nil)

  private def kindForClasslike(classDef: ClassDef): Kind =
    def typeArgs = classDef.getTypeParams.map(mkTypeArgument(_))

    def parameterModifier(parameter: Symbol): String =
      val fieldSymbol = classDef.symbol.declaredField(parameter.normalizedName)
      def isVal = fieldSymbol.flags.is(Flags.ParamAccessor) &&
        !classDef.symbol.flags.is(Flags.Case) &&
        !fieldSymbol.flags.is(Flags.Private)

      if fieldSymbol.flags.is(Flags.Mutable) then "var "
      else if isVal then "val "
      else ""

    def args = if constructorWithoutParamLists(classDef) then Nil else
      val constr =
        Some(classDef.constructor.symbol)
          .filter(s => s.exists && !s.isHiddenByVisibility)
          .map( _.tree.asInstanceOf[DefDef])
      constr.fold(Nil)(
        _.termParamss.map(pList => ParametersList(pList.params.map(p => mkParameter(p, parameterModifier)), paramListModifier(pList.params)))
        )

    if classDef.symbol.flags.is(Flags.Module) then Kind.Object
    else if classDef.symbol.flags.is(Flags.Trait) then
      Kind.Trait(typeArgs, args)
    else if classDef.symbol.flags.is(Flags.Enum) && classDef.symbol.flags.is(Flags.Case) then Kind.EnumCase(Kind.Class(typeArgs, args))
    else if classDef.symbol.flags.is(Flags.Enum) then Kind.Enum(typeArgs, args)
    else Kind.Class(typeArgs, args)

  def mkClass(classDef: ClassDef)(
    dri: DRI = classDef.symbol.dri,
    name: String = classDef.symbol.normalizedName,
    signatureOnly: Boolean = false,
    modifiers: Seq[Modifier] = classDef.symbol.getExtraModifiers(),
  ): Member =
    def unpackTreeToClassDef(tree: Tree): ClassDef = tree match
      case tree: ClassDef => tree
      case TypeDef(_, tbt: TypeBoundsTree) => unpackTreeToClassDef(tbt.tpe.typeSymbol.tree)
      case TypeDef(_, tt: TypeTree) => unpackTreeToClassDef(tt.tpe.typeSymbol.tree)
      case c: Apply =>
        c.symbol.owner.tree.symbol.tree match
          case tree: ClassDef => tree
      case tt: TypeTree => unpackTreeToClassDef(tt.tpe.typeSymbol.tree)

    def getSupertypesGraph(classDef: Tree, link: LinkToType): Seq[(LinkToType, LinkToType)] =
      val smbl = classDef.symbol
      val parents = unpackTreeToClassDef(classDef).parents
      parents.flatMap { case tree =>
        val symbol = if tree.symbol.isClassConstructor then tree.symbol.owner else tree.symbol
        val superLink = LinkToType(tree.asSignature, symbol.dri, bareClasslikeKind(symbol))
        Seq(link -> superLink) ++ getSupertypesGraph(tree, superLink)
      }

    val supertypes = getSupertypes(using qctx)(classDef).map {
      case (symbol, tpe) =>
        LinkToType(tpe.asSignature, symbol.dri, bareClasslikeKind(symbol))
    }
    val selfSiangture: DSignature = typeForClass(classDef).asSignature

    val graph = HierarchyGraph.withEdges(
      getSupertypesGraph(classDef, LinkToType(selfSiangture, classDef.symbol.dri, bareClasslikeKind(classDef.symbol)))
    )

    val baseMember = mkMember(classDef.symbol, kindForClasslike(classDef), selfSiangture)(
      modifiers = modifiers,
      graph = graph,
      deprecated = classDef.symbol.isDeprecated()
    )

    if summon[DocContext].args.generateInkuire then {

      val classType = classDef.asInkuire(Set.empty)
      val variableNames = classType.params.map(_.typ.name.name).toSet

      val parents = classDef.parents.map(_.asInkuire(variableNames))

      val isModule = classDef.symbol.flags.is(Flags.Module)

      if !isModule then Inkuire.db = Inkuire.db.copy(types = Inkuire.db.types.updated(classType.itid.get, (classType, parents)))

      classDef.symbol.declaredTypes.foreach {
        case typeSymbol: Symbol =>
          val typeDef = typeSymbol.tree.asInstanceOf[TypeDef]
          if typeDef.rhs.symbol.fullName.contains("java") then
            val t = typeSymbol.tree.asInkuire(variableNames) // TODO [Inkuire] Hack until type aliases are supported
            val tJava = typeDef.rhs.symbol.tree.asInkuire(variableNames)
            Inkuire.db = Inkuire.db.copy(types = Inkuire.db.types.updated(t.itid.get, (t, Seq.empty))) // TODO [Inkuire] Hack until type aliases are supported
            Inkuire.db = Inkuire.db.copy(types = Inkuire.db.types.updated(tJava.itid.get, (tJava, Seq.empty)))
      }

      classDef.symbol.declaredMethods
        .filter { (s: Symbol) =>
          !s.flags.is(Flags.Private) &&
            !s.flags.is(Flags.Protected) &&
            !s.flags.is(Flags.Override)
        }
        .foreach {
          case implicitConversion: Symbol if implicitConversion.flags.is(Flags.Implicit)
                                          && classDef.symbol.flags.is(Flags.Module)
                                          && implicitConversion.owner.fullName == ("scala.Predef$") =>
            val defdef = implicitConversion.tree.asInstanceOf[DefDef]
            val to = defdef.returnTpt.asInkuire(variableNames)
            val from = defdef.paramss.flatMap(_.params).collectFirst {
              case v: ValDef => v.tpt.asInkuire(variableNames)
            }
            from match
              case Some(from) => Inkuire.db = Inkuire.db.copy(implicitConversions = Inkuire.db.implicitConversions :+ (from.itid.get -> to))
              case None =>

          case methodSymbol: Symbol =>
            val defdef = methodSymbol.tree.asInstanceOf[DefDef]
            val methodVars = defdef.paramss.flatMap(_.params).collect {
              case TypeDef(name, _) => name
            }
            val vars = variableNames ++ methodVars
            val receiver: Option[Inkuire.Type] =
              Some(classType)
                .filter(_ => !isModule)
                .orElse(methodSymbol.extendedSymbol.flatMap(s => partialAsInkuire(vars).lift(s.tpt)))
            val sgn = Inkuire.ExternalSignature(
              signature = Inkuire.Signature(
                receiver = receiver,
                arguments = methodSymbol.nonExtensionParamLists.flatMap(_.params).collect {
                  case ValDef(_, tpe, _) => tpe.asInkuire(vars)
                },
                result = defdef.returnTpt.asInkuire(vars),
                context = Inkuire.SignatureContext(
                  vars = vars.toSet,
                  constraints = Map.empty //TODO [Inkuire] Type bounds
                )
              ),
              name = methodSymbol.name,
              packageName = methodSymbol.dri.location,
              uri = methodSymbol.dri.externalLink.getOrElse("")
            )
            Inkuire.db = Inkuire.db.copy(functions = Inkuire.db.functions :+ sgn)
      }

    }

    if signatureOnly then baseMember else baseMember.copy(
        members = classDef.extractPatchedMembers.sortBy(m => (m.name, m.kind.name)),
        directParents = classDef.getParentsAsLinkToTypes,
        parents = supertypes,
        companion = classDef.getCompanion
    )

  private val conversionSymbol = Symbol.requiredClass("scala.Conversion")

  def extractImplicitConversion(tpe: TypeRepr): Option[ImplicitConversion] =
      if tpe.derivesFrom(conversionSymbol) then tpe.baseType(conversionSymbol) match
        case AppliedType(tpe, List(from: TypeRepr, to: TypeRepr)) =>
          Some(ImplicitConversion(from.typeSymbol.dri, to.typeSymbol.dri))
        case _ =>
          None
      else None

  private def isDocumentableExtension(s: Symbol) =
    !s.isHiddenByVisibility && !s.isSyntheticFunc && s.isExtensionMethod

  private def parseMember(c: ClassDef)(s: Tree): Option[Member] = processTreeOpt(s)(s match
      case dd: DefDef if isDocumentableExtension(dd.symbol) =>
        dd.symbol.extendedSymbol.map { extSym =>
          val target = ExtensionTarget(
            extSym.symbol.normalizedName,
            extSym.tpt.asSignature,
            extSym.tpt.symbol.dri,
            extSym.symbol.pos.get.start
          )
          parseMethod(c, dd.symbol,specificKind = Kind.Extension(target, _))
        }
      // TODO check given methods?
      case dd: DefDef if !dd.symbol.isHiddenByVisibility && dd.symbol.isGiven && !dd.symbol.isArtifact =>
        Some(dd.symbol.owner.memberType(dd.name))
          .filterNot(_.exists)
          .map { _ =>
            parseMethod(c, dd.symbol, specificKind =
              Kind.Given(_, getGivenInstance(dd), None)
            )
          }

      case dd: DefDef if !dd.symbol.isHiddenByVisibility && dd.symbol.isExported && !dd.symbol.isArtifact =>
        val exportedTarget = dd.rhs.collect {
          case a: Apply => a.fun.asInstanceOf[Select]
          case s: Select => s
        }
        val functionName = exportedTarget.fold("function")(_.name)
        val instanceName = exportedTarget.collect {
          case Select(qualifier: Select, _) => qualifier.name
          case Select(qualifier: Ident, _) => qualifier.tpe.typeSymbol.normalizedName
        }.getOrElse("instance")
        val dri = dd.rhs.collect {
          case s: Select if s.symbol.isDefDef => s.symbol.dri
        }.orElse(exportedTarget.map(_.qualifier.tpe.typeSymbol.dri))

        Some(parseMethod(c, dd.symbol, specificKind = Kind.Exported(_))
          .withOrigin(Origin.ExportedFrom(s"$instanceName.$functionName", dri)))

      case dd: DefDef if !dd.symbol.isHiddenByVisibility && !dd.symbol.isGiven && !dd.symbol.isSyntheticFunc && !dd.symbol.isExtensionMethod && !dd.symbol.isArtifact =>
        Some(parseMethod(c, dd.symbol))

      case td: TypeDef if !td.symbol.flags.is(Flags.Synthetic) && (!td.symbol.flags.is(Flags.Case) || !td.symbol.flags.is(Flags.Enum)) =>
        Some(parseTypeDef(td))

      case vd: ValDef if !isSyntheticField(vd.symbol)
        && (!vd.symbol.flags.is(Flags.Case) || !vd.symbol.flags.is(Flags.Enum))
        && vd.symbol.isGiven =>
          val classDef = Some(vd.tpt.tpe).flatMap(_.classSymbol.map(_.tree.asInstanceOf[ClassDef]))
          Some(classDef.filter(_.symbol.flags.is(Flags.Module)).fold[Member](parseValDef(c, vd))(parseGivenClasslike(_)))

      case vd: ValDef if !isSyntheticField(vd.symbol) && (!vd.symbol.flags.is(Flags.Case) || !vd.symbol.flags.is(Flags.Enum)) =>
        Some(parseValDef(c, vd))

      case c: ClassDef if c.symbol.owner.memberMethod(c.name).exists(_.flags.is(Flags.Given)) =>
        Some(parseGivenClasslike(c))

      case c: ClassDef if c.symbol.shouldDocumentClasslike &&  !c.symbol.isGiven =>
        Some(parseClasslike(c))

      case _ => None
  )

  private def parseGivenClasslike(c: ClassDef): Member = {
    val parsedClasslike = parseClasslike(c)

    val parentTpe = c.parents(0) match {
      case t: TypeTree => Some(t.tpe)
      case t: Term => Some(t.tpe)
      case _ => None
    }

    val givenParents = parsedClasslike.directParents.headOption
    val cls: Kind.Class = parsedClasslike.kind match
      case Kind.Object => Kind.Class(Nil, Nil)
      case Kind.Trait(tps, args) => Kind.Class(tps, args)
      case cls: Kind.Class => cls
      case other =>
        report.warning("Unrecoginzed kind for given: $other", c.pos)
        Kind.Class(Nil, Nil)

    parsedClasslike.withKind(
      Kind.Given(cls, givenParents.map(_.signature), parentTpe.flatMap(extractImplicitConversion))
    )
  }

  private def parseInheritedMember(c: ClassDef)(s: Tree): Option[Member] =
    def inheritance = Some(InheritedFrom(s.symbol.owner.normalizedName, s.symbol.dri))
    processTreeOpt(s)(s match
      case c: ClassDef if c.symbol.shouldDocumentClasslike && !c.symbol.isGiven => Some(parseClasslike(c, signatureOnly = true))
      case c: ClassDef if c.symbol.owner.memberMethod(c.name).exists(_.flags.is(Flags.Given)) => Some(parseGivenClasslike(c))
      case other => {
        val parsed = parseMember(c)(other)
        parsed.map(p =>
          val parentDRI = c.symbol.dri
          p.copy(
            dri = p.dri.copy(
              location = parentDRI.location,
              externalLink = None
            )
          )
        )
      }
    ).map(_.copy(inheritedFrom = inheritance))

  extension (using Quotes)(c: reflect.ClassDef)

    def membersToDocument = c.body.filterNot(_.symbol.isHiddenByVisibility)

    def getNonTrivialInheritedMemberTrees =
      c.symbol.getmembers.filterNot(s => s.isHiddenByVisibility || s.maybeOwner == c.symbol)
        .filter(s => s.maybeOwner != defn.ObjectClass && s.maybeOwner != defn.AnyClass)
        .map(_.tree)

  extension (c: ClassDef)
    def extractMembers: Seq[Member] = {
      val inherited = c.getNonTrivialInheritedMemberTrees.collect {
        case dd: DefDef if !dd.symbol.isClassConstructor && !(dd.symbol.isSuperBridgeMethod || dd.symbol.isDefaultHelperMethod) => dd
        case other => other
      }
      c.membersToDocument.flatMap(parseMember(c)) ++
        inherited.flatMap(s => parseInheritedMember(c)(s))
    }

    /** Extracts members while taking Dotty logic for patching the stdlib into account. */
    def extractPatchedMembers: Seq[Member] = {
      val ownMembers = c.extractMembers
      def extractPatchMembers(sym: Symbol) = {
        // NOTE for some reason scala.language$.experimental$ class doesn't show up here, so we manually add the name
        val ownMemberDRIs = ownMembers.iterator.map(_.name).toSet + "experimental$"
        sym.tree.asInstanceOf[ClassDef]
          .membersToDocument.filterNot(m => ownMemberDRIs.contains(m.symbol.name))
          .flatMap(parseMember(c))
      }
      c.symbol.fullName match {
        case "scala.Predef$" =>
          ownMembers ++
          extractPatchMembers(qctx.reflect.Symbol.requiredClass("scala.runtime.stdLibPatches.Predef$"))
        case "scala.language$" =>
          ownMembers ++
          extractPatchMembers(qctx.reflect.Symbol.requiredModule("scala.runtime.stdLibPatches.language").moduleClass)
        case "scala.language$.experimental$" =>
          ownMembers ++
          extractPatchMembers(qctx.reflect.Symbol.requiredModule("scala.runtime.stdLibPatches.language.experimental").moduleClass)
        case _ => ownMembers
      }

    }

    def getTreeOfFirstParent: Option[Tree] =
      c.getParentsAsTreeSymbolTuples.headOption.map(_._1)

    def getParentsAsLinkToTypes: List[LinkToType] =
      c.getParentsAsTreeSymbolTuples.map {
        (tree, symbol) => LinkToType(tree.asSignature, symbol.dri, bareClasslikeKind(symbol))
      }

    def getParentsAsTreeSymbolTuples: List[(Tree, Symbol)] =
      for
        parentTree <- c.parents if isValidPos(parentTree.pos)  // We assume here that order is correct
        parentSymbol = if parentTree.symbol.isClassConstructor then parentTree.symbol.owner else parentTree.symbol
        if parentSymbol != defn.ObjectClass && parentSymbol != defn.AnyClass
      yield (parentTree, parentSymbol)

    def getConstructors: List[Symbol] = c.membersToDocument.collect {
      case d: DefDef if d.symbol.isClassConstructor && c.constructor.symbol != d.symbol => d.symbol
    }.toList

    def getParameterModifier(parameter: Symbol): String =
      val fieldSymbol = c.symbol.declaredField(parameter.normalizedName)
      if fieldSymbol.flags.is(Flags.Mutable) then "var "
      else if fieldSymbol.flags.is(Flags.ParamAccessor) && !c.symbol.flags.is(Flags.Case) && !fieldSymbol.flags.is(Flags.Private) then "val "
      else ""

    def getTypeParams: List[TypeDef] =
      c.body.collect { case targ: TypeDef => targ  }.filter(_.symbol.isTypeParam)

    def getCompanion: Option[DRI] = c.symbol.getCompanionSymbol
      .filter(!_.flags.is(Flags.Synthetic))
      .filterNot(_.isHiddenByVisibility)
      .map(_.dri)


  def parseClasslike(classDef: ClassDef, signatureOnly: Boolean = false): Member = classDef match
    case c: ClassDef if classDef.symbol.flags.is(Flags.Module) => parseObject(c, signatureOnly)
    case c: ClassDef if classDef.symbol.flags.is(Flags.Enum) && !classDef.symbol.flags.is(Flags.Case) => parseEnum(c, signatureOnly)
    case clazz => mkClass(classDef)(signatureOnly = signatureOnly)

  def parseObject(classDef: ClassDef, signatureOnly: Boolean = false): Member =
    mkClass(classDef)(
      // All objects are final so we do not need final modifier!
      modifiers = classDef.symbol.getExtraModifiers().filter(_ != Modifier.Final),
      signatureOnly = signatureOnly
    )

  def parseEnum(classDef: ClassDef, signatureOnly: Boolean = false): Member =
    val extraModifiers = classDef.symbol.getExtraModifiers().filter(_ != Modifier.Sealed).filter(_ != Modifier.Abstract)
    val companion = classDef.symbol.getCompanionSymbol.map(_.tree.asInstanceOf[ClassDef]).get

    val enumVals = companion.membersToDocument.collect {
      case vd: ValDef if !isSyntheticField(vd.symbol) && vd.symbol.flags.is(Flags.Enum) && vd.symbol.flags.is(Flags.Case) => vd
    }.toList.map(parseValDef(classDef, _))

    val enumTypes = companion.membersToDocument.collect {
      case td: TypeDef if !td.symbol.flags.is(Flags.Synthetic) && td.symbol.flags.is(Flags.Enum) && td.symbol.flags.is(Flags.Case) => td
    }.toList.map(parseTypeDef)

    val enumNested = companion.membersToDocument.collect {
      case c: ClassDef if c.symbol.flags.is(Flags.Case) && c.symbol.flags.is(Flags.Enum) => processTree(c)(parseClasslike(c))
    }.flatten

    val enumClass = mkClass(classDef)(modifiers = extraModifiers, signatureOnly = signatureOnly)

    val cases = (
      enumNested ++
      enumTypes ++
      enumVals.map(m => m.copy(dri = m.dri.copy(location = enumClass.dri.location)))
    )

    enumClass.withMembers(cases)

  def parseMethod(
      c: ClassDef,
      methodSymbol: Symbol,
      emptyParamsList: Boolean = false,
      paramPrefix: Symbol => String = _ => "",
      specificKind: (Kind.Def => Kind) = identity
    ): Member =
    val method = methodSymbol.tree.asInstanceOf[DefDef]
    val paramLists: List[TermParamClause] = methodSymbol.nonExtensionParamLists
    val genericTypes = if (methodSymbol.isClassConstructor) Nil else method.leadingTypeParams

    val memberInfo = unwrapMemberInfo(c, methodSymbol)

    val basicKind: Kind.Def = Kind.Def(
      genericTypes.map(mkTypeArgument(_, memberInfo.genericTypes)),
      paramLists.zipWithIndex.map { (pList, index) =>
        ParametersList(pList.params.map(mkParameter(_, paramPrefix, memberInfo = memberInfo.paramLists(index))), paramListModifier(pList.params))
      }
    )

    val methodKind =
      if methodSymbol.isClassConstructor then Kind.Constructor(basicKind)
      else if methodSymbol.flags.is(Flags.Implicit) then extractImplicitConversion(method.returnTpt.tpe) match
        case Some(conversion) if paramLists.size == 0 || (paramLists.size == 1 && paramLists.head.params.size == 0) =>
          Kind.Implicit(basicKind, Some(conversion))
        case None if paramLists.size == 1 && paramLists(0).params.size == 1 =>
          Kind.Implicit(basicKind, Some(
            ImplicitConversion(
              paramLists(0).params(0).tpt.tpe.typeSymbol.dri,
              method.returnTpt.tpe.typeSymbol.dri
            )
          ))
        case _ =>
          Kind.Implicit(basicKind, None)
      else specificKind(basicKind)

    val origin = if !methodSymbol.isOverridden then Origin.RegularlyDefined else
      val overriddenSyms = methodSymbol.allOverriddenSymbols.map(_.owner)
      Origin.Overrides(overriddenSyms.map(s => Overridden(s.name, s.dri)).toSeq)

    mkMember(methodSymbol, methodKind, memberInfo.res.asSignature)(origin = origin, deprecated = methodSymbol.isDeprecated())

  def mkParameter(
    argument: ValDef,
    prefix: Symbol => String = _ => "",
    isExtendedSymbol: Boolean = false,
    isGrouped: Boolean = false,
    memberInfo: Map[String, TypeRepr] = Map.empty) =
      val inlinePrefix = if argument.symbol.flags.is(Flags.Inline) then "inline " else ""
      val nameIfNotSynthetic = Option.when(!argument.symbol.flags.is(Flags.Synthetic))(argument.symbol.normalizedName)
      val name = argument.symbol.normalizedName
      Parameter(
        argument.symbol.getAnnotations(),
        inlinePrefix + prefix(argument.symbol),
        nameIfNotSynthetic,
        argument.symbol.dri,
        memberInfo.get(name).fold(argument.tpt.asSignature)(_.asSignature),
        isExtendedSymbol,
        isGrouped
      )

  def mkTypeArgument(argument: TypeDef, memberInfo: Map[String, TypeBounds] = Map.empty): TypeParameter =
    val variancePrefix: "+" | "-" | "" =
      if  argument.symbol.flags.is(Flags.Covariant) then "+"
      else if argument.symbol.flags.is(Flags.Contravariant) then "-"
      else ""

    val name = argument.symbol.normalizedName
    val normalizedName = if name.matches("_\\$\\d*") then "_" else name
    TypeParameter(
      argument.symbol.getAnnotations(),
      variancePrefix,
      normalizedName,
      argument.symbol.dri,
      memberInfo.get(name).fold(argument.rhs.asSignature)(_.asSignature)
    )

  def parseTypeDef(typeDef: TypeDef): Member =
    def isTreeAbstract(typ: Tree): Boolean = typ match {
      case TypeBoundsTree(_, _) => true
      case LambdaTypeTree(params, body) => isTreeAbstract(body)
      case _ => false
    }
    val (generics, tpeTree) = typeDef.rhs match
      case LambdaTypeTree(params, body) => (params.map(mkTypeArgument(_)), body)
      case tpe => (Nil, tpe)

    val defaultKind = Kind.Type(!isTreeAbstract(typeDef.rhs), typeDef.symbol.isOpaque, generics).asInstanceOf[Kind.Type]
    val kind = if typeDef.symbol.flags.is(Flags.Enum) then Kind.EnumCase(defaultKind)
      else defaultKind
    mkMember(typeDef.symbol, kind, tpeTree.asSignature)(deprecated = typeDef.symbol.isDeprecated())

  def parseValDef(c: ClassDef, valDef: ValDef): Member =
    def defaultKind = if valDef.symbol.flags.is(Flags.Mutable) then Kind.Var else Kind.Val
    val memberInfo = unwrapMemberInfo(c, valDef.symbol)
    val kind = if valDef.symbol.flags.is(Flags.Implicit) then
        Kind.Implicit(Kind.Val, extractImplicitConversion(valDef.tpt.tpe))
        else if valDef.symbol.flags.is(Flags.Enum) then Kind.EnumCase(Kind.Val)
        else defaultKind

    mkMember(valDef.symbol, kind, memberInfo.res.asSignature)(deprecated = valDef.symbol.isDeprecated())

  def mkMember(symbol: Symbol, kind: Kind, signature: DSignature)(
    modifiers: Seq[Modifier] = symbol.getExtraModifiers(),
    origin: Origin = Origin.RegularlyDefined,
    inheritedFrom: Option[InheritedFrom] = None,
    graph: HierarchyGraph = HierarchyGraph.empty,
    deprecated: Option[Annotation] = None,
  ) = Member(
      name = symbol.normalizedName,
      dri = symbol.dri,
      kind = kind,
      visibility = symbol.getVisibility(),
      modifiers = modifiers,
      annotations = symbol.getAnnotations(),
      signature = signature,
      sources = symbol.source,
      origin = origin,
      inheritedFrom = inheritedFrom,
      graph = graph,
      docs = symbol.documentation,
      deprecated = deprecated
    )

  case class MemberInfo(genericTypes: Map[String, TypeBounds], paramLists: List[Map[String, TypeRepr]], res: TypeRepr)

  def unwrapMemberInfo(c: ClassDef, symbol: Symbol): MemberInfo =
    val baseTypeRepr = memberInfo(c, symbol)

    def handlePolyType(polyType: PolyType): MemberInfo =
      MemberInfo(polyType.paramNames.zip(polyType.paramBounds).toMap, List.empty, polyType.resType)

    def handleMethodType(memberInfo: MemberInfo, methodType: MethodType): MemberInfo =
      MemberInfo(memberInfo.genericTypes, memberInfo.paramLists ++ List(methodType.paramNames.zip(methodType.paramTypes).toMap), methodType.resType)

    def handleByNameType(memberInfo: MemberInfo, byNameType: ByNameType): MemberInfo =
      MemberInfo(memberInfo.genericTypes, memberInfo.paramLists, byNameType.underlying)

    def recursivelyCalculateMemberInfo(memberInfo: MemberInfo): MemberInfo = memberInfo.res match
      case p: PolyType => recursivelyCalculateMemberInfo(handlePolyType(p))
      case m: MethodType => recursivelyCalculateMemberInfo(handleMethodType(memberInfo, m))
      case b: ByNameType => handleByNameType(memberInfo, b)
      case _ => memberInfo

    recursivelyCalculateMemberInfo(MemberInfo(Map.empty, List.empty, baseTypeRepr))

  private def paramListModifier(parameters: Seq[ValDef]): String =
    if parameters.size > 0 then
      if parameters(0).symbol.flags.is(Flags.Given) then "using "
      else if parameters(0).symbol.flags.is(Flags.Implicit) then "implicit "
      else ""
    else ""