package dotty.tools.scaladoc.tasty

import collection.JavaConverters._
import dotty.tools.scaladoc._
import dotty.tools.scaladoc.{Signature => DSignature}
import dotty.tools.scaladoc.Inkuire

import scala.quoted._

import SymOps._
import NameNormalizer._
import SyntheticsSupport._
import dotty.tools.dotc.core.NameKinds

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

    def getSupertypesGraph(link: LinkToType, to: Seq[Tree]): Seq[(LinkToType, LinkToType)] =
      to.flatMap { case tree =>
        val symbol = if tree.symbol.isClassConstructor then tree.symbol.owner else tree.symbol
        val superLink = LinkToType(tree.asSignature, symbol.dri, bareClasslikeKind(symbol))
        val nextTo = unpackTreeToClassDef(tree).parents
        if symbol.isHiddenByVisibility then getSupertypesGraph(link, nextTo)
        else Seq(link -> superLink) ++ getSupertypesGraph(superLink, nextTo)
      }

    val supertypes = getSupertypes(using qctx)(classDef)
      .filterNot((s, t) => s.isHiddenByVisibility)
      .map {
        case (symbol, tpe) =>
          LinkToType(tpe.asSignature, symbol.dri, bareClasslikeKind(symbol))
      }
    val selfType = classDef.self.map { (valdef: ValDef) =>
      val symbol = valdef.symbol
      val tpe = valdef.tpt.tpe
      LinkToType(tpe.asSignature, symbol.dri, Kind.Type(false, false, Seq.empty))
    }
    val selfSignature: DSignature = typeForClass(classDef).asSignature

    val graph = HierarchyGraph.withEdges(
      getSupertypesGraph(LinkToType(selfSignature, classDef.symbol.dri, bareClasslikeKind(classDef.symbol)), unpackTreeToClassDef(classDef).parents)
    )

    val baseMember = mkMember(classDef.symbol, kindForClasslike(classDef), selfSignature)(
      modifiers = modifiers,
      graph = graph,
      deprecated = classDef.symbol.isDeprecated(),
    ).copy(
      directParents = classDef.getParentsAsLinkToTypes,
      parents = supertypes,
    )

    if summon[DocContext].args.generateInkuire then doInkuireStuff(classDef)

    if signatureOnly then baseMember else baseMember.copy(
        members = classDef.extractPatchedMembers.sortBy(m => (m.name, m.kind.name)),
        selfType = selfType,
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

  private def parseMember(c: ClassDef)(s: Tree): Option[Member] = processTreeOpt(s) { s match
      case dd: DefDef if isDocumentableExtension(dd.symbol) =>
        dd.symbol.extendedSymbol.map { extSym =>
          val memberInfo = unwrapMemberInfo(c, dd.symbol)
          val typeParams = dd.symbol.extendedTypeParams.map(mkTypeArgument(_, memberInfo.genericTypes))
          val termParams = dd.symbol.extendedTermParamLists.zipWithIndex.flatMap { case (paramList, index) =>
            memberInfo.paramLists(index) match
              case EvidenceOnlyParameterList => Nil
              case info: RegularParameterList =>
                Seq(ParametersList(paramList.params.map(mkParameter(_, memberInfo = info)), paramListModifier(paramList.params)))
          }
          val target = ExtensionTarget(
            extSym.symbol.normalizedName,
            typeParams,
            termParams,
            extSym.tpt.asSignature,
            extSym.tpt.symbol.dri,
            extSym.symbol.pos.get.start
          )
          parseMethod(c, dd.symbol,specificKind = Kind.Extension(target, _))
        }

      case dd: DefDef if !dd.symbol.isHiddenByVisibility && dd.symbol.isExported && !dd.symbol.isArtifact =>
        dd.rhs.map {
          case TypeApply(rhs, _) => rhs
          case Apply(TypeApply(rhs, _), _) => rhs
          case rhs => rhs
        }.map(_.tpe.termSymbol).filter(_.exists).map(_.tree).map {
          case v: ValDef if v.symbol.flags.is(Flags.Module) && !v.symbol.flags.is(Flags.Synthetic) =>
            v.symbol.owner -> Symbol.newVal(c.symbol, dd.name, v.tpt.tpe, Flags.Final, Symbol.noSymbol).tree
          case other => other.symbol.owner -> other
        }.flatMap { (originalOwner, tree) =>
          parseMember(c)(tree)
            .map { m => m
              .withDRI(dd.symbol.dri)
              .withName(dd.symbol.normalizedName)
              .withKind(Kind.Exported(m.kind))
              .withOrigin(Origin.ExportedFrom(Some(Link(originalOwner.normalizedName, originalOwner.dri))))
            }
        }

      case dd: DefDef if !dd.symbol.isHiddenByVisibility && !dd.symbol.isSyntheticFunc && !dd.symbol.isExtensionMethod && !dd.symbol.isArtifact =>
        Some(parseMethod(c, dd.symbol))

      case td: TypeDef if !td.symbol.flags.is(Flags.Synthetic) && (!td.symbol.flags.is(Flags.Case) || !td.symbol.flags.is(Flags.Enum)) =>
        Some(parseTypeDef(td))

      case vd: ValDef if !isSyntheticField(vd.symbol) && (!vd.symbol.flags.is(Flags.Case) || !vd.symbol.flags.is(Flags.Enum)) =>
        Some(parseValDef(c, vd))

      case c: ClassDef if c.symbol.shouldDocumentClasslike =>
        Some(parseClasslike(c))

      case _ => None
  }

  private def parseInheritedMember(c: ClassDef)(s: Tree): Option[Member] =
    def inheritance = Some(InheritedFrom(s.symbol.owner.normalizedName, s.symbol.dri))
    processTreeOpt(s)(s match
      case c: ClassDef if c.symbol.shouldDocumentClasslike => Some(parseClasslike(c, signatureOnly = true))
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
        parentSymbol = parentTree match
          case t: TypeTree => t.tpe.typeSymbol
          case tree if tree.symbol.isClassConstructor => tree.symbol.owner
          case tree => tree.symbol
        if parentSymbol != defn.ObjectClass && parentSymbol != defn.AnyClass && !parentSymbol.isHiddenByVisibility
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
    val paramLists: List[TermParamClause] = methodSymbol.nonExtensionTermParamLists
    val genericTypes: List[TypeDef] = if (methodSymbol.isClassConstructor) Nil else methodSymbol.nonExtensionLeadingTypeParams

    val memberInfo = unwrapMemberInfo(c, methodSymbol)

    val basicKind: Kind.Def = Kind.Def(
      genericTypes.map(mkTypeArgument(_, memberInfo.genericTypes, memberInfo.contextBounds)),
      paramLists.zipWithIndex.flatMap { (pList, index) =>
        memberInfo.paramLists(index) match
          case EvidenceOnlyParameterList => Nil
          case info: RegularParameterList =>
            Seq(ParametersList(pList.params.map(
              mkParameter(_, paramPrefix, memberInfo = info)), paramListModifier(pList.params)
            ))
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
      else if methodSymbol.flags.is(Flags.Given) then Kind.Given(basicKind, Some(method.returnTpt.tpe.asSignature), extractImplicitConversion(method.returnTpt.tpe))
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

  def mkTypeArgument(
    argument: TypeDef,
    memberInfo: Map[String, TypeBounds] = Map.empty,
    contextBounds: Map[String, DSignature] = Map.empty
    ): TypeParameter =
    val variancePrefix: "+" | "-" | "" =
      if  argument.symbol.flags.is(Flags.Covariant) then "+"
      else if argument.symbol.flags.is(Flags.Contravariant) then "-"
      else ""

    val name = argument.symbol.normalizedName
    val normalizedName = if name.matches("_\\$\\d*") then "_" else name
    val boundsSignature = memberInfo.get(name).fold(argument.rhs.asSignature)(_.asSignature)
    val signature = contextBounds.get(name) match
      case None => boundsSignature
      case Some(contextBoundsSignature) =>
        boundsSignature ++ DSignature(Plain(" : ")) ++ contextBoundsSignature

    TypeParameter(
      argument.symbol.getAnnotations(),
      variancePrefix,
      normalizedName,
      argument.symbol.dri,
      signature
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

    if typeDef.symbol.flags.is(Flags.Exported)
    then {
      val origin = Some(tpeTree).flatMap {
        case TypeBoundsTree(l: TypeTree, h: TypeTree) if l.tpe == h.tpe =>
          Some(Link(l.tpe.typeSymbol.owner.name, l.tpe.typeSymbol.owner.dri))
        case _ => None
      }
      mkMember(typeDef.symbol, Kind.Exported(kind), tpeTree.asSignature)(deprecated = typeDef.symbol.isDeprecated(), origin = Origin.ExportedFrom(origin))
    }
    else mkMember(typeDef.symbol, kind, tpeTree.asSignature)(deprecated = typeDef.symbol.isDeprecated())

  def parseValDef(c: ClassDef, valDef: ValDef): Member =
    def defaultKind = if valDef.symbol.flags.is(Flags.Mutable) then Kind.Var else Kind.Val
    val memberInfo = unwrapMemberInfo(c, valDef.symbol)
    val kind = if valDef.symbol.flags.is(Flags.Implicit) then Kind.Implicit(Kind.Val, extractImplicitConversion(valDef.tpt.tpe))
      else if valDef.symbol.flags.is(Flags.Given) then Kind.Given(Kind.Val, Some(memberInfo.res.asSignature), extractImplicitConversion(valDef.tpt.tpe))
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

  object EvidenceOnlyParameterList
  type RegularParameterList = Map[String, TypeRepr]
  type ParameterList = RegularParameterList | EvidenceOnlyParameterList.type

  case class MemberInfo(
    genericTypes: Map[String, TypeBounds],
    paramLists: List[ParameterList],
    res: TypeRepr,
    contextBounds: Map[String, DSignature] = Map.empty,
  )


  def unwrapMemberInfo(c: ClassDef, symbol: Symbol): MemberInfo =
    val baseTypeRepr = memberInfo(c, symbol)

    def isSyntheticEvidence(name: String) =
      if !name.startsWith(NameKinds.EvidenceParamName.separator) then false else
        // This assumes that every parameter that starts with `evidence$` and is implicit is generated by compiler to desugar context bound.
        // Howrever, this is just a heuristic, so
        // `def foo[A](evidence$1: ClassTag[A]) = 1`
        // will be documented as
        // `def foo[A: ClassTag] = 1`.
        // Scala spec states that `$` should not be used in names and behaviour may be undefiend in such case.
        // Documenting method slightly different then its definition is withing the 'undefiend behaviour'.
        symbol.paramSymss.flatten.find(_.name == name).exists(_.flags.is(Flags.Implicit))

    def handlePolyType(polyType: PolyType): MemberInfo =
      MemberInfo(polyType.paramNames.zip(polyType.paramBounds).toMap, List.empty, polyType.resType)

    def handleMethodType(memberInfo: MemberInfo, methodType: MethodType): MemberInfo =
      val rawParams = methodType.paramNames.zip(methodType.paramTypes).toMap
      val (evidences, notEvidences) = rawParams.partition(e => isSyntheticEvidence(e._1))


      def findParamRefs(t: TypeRepr): Seq[ParamRef] = t match
        case paramRef: ParamRef => Seq(paramRef)
        case AppliedType(_, args) => args.flatMap(findParamRefs)
        case MatchType(bound, scrutinee,  cases) =>
            findParamRefs(bound) ++ findParamRefs(scrutinee)
        case _ => Nil

      def nameForRef(ref: ParamRef): String =
        val PolyType(names, _, _) = ref.binder
        names(ref.paramNum)

      val (paramsThatLookLikeContextBounds, contextBounds) =
        evidences.partitionMap {
          case (_, AppliedType(tpe, List(typeParam: ParamRef))) =>
            Right(nameForRef(typeParam) -> tpe.asSignature)
          case (name, original) =>
            findParamRefs(original) match
              case Nil => Left((name, original))
              case typeParam :: _ =>
                val name = nameForRef(typeParam)
                val signature = Seq(Plain("(["), dotty.tools.scaladoc.Type(name, None), Plain("]"), Keyword(" =>> ")) ++ original.asSignature ++ Seq(Plain(")"))
                Right(name -> signature.toList)
        }

      val newParams = notEvidences ++ paramsThatLookLikeContextBounds

      val newLists: List[ParameterList] = if newParams.isEmpty   && contextBounds.nonEmpty
        then memberInfo.paramLists ++  Seq(EvidenceOnlyParameterList)
        else memberInfo.paramLists ++ Seq(newParams)

      MemberInfo(memberInfo.genericTypes, newLists , methodType.resType, contextBounds.toMap)

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