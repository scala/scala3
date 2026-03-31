package dotty.tools.scaladoc.tasty

import dotty.tools.scaladoc._
import dotty.tools.scaladoc.{Signature => DSignature}

import dotty.tools.scaladoc.cc.*

import scala.quoted._

import SymOps._
import NameNormalizer._
import SyntheticsSupport._
import dotty.tools.dotc.core.NameKinds

// Please use this only for things defined in the api.scala file
import dotty.tools.{scaladoc => api}

trait ClassLikeSupport:
  self: TastyParser =>
  import qctx.reflect._

  private given qctx.type = qctx

  extension (symbol: Symbol) {
    def getExtraModifiers(): Seq[Modifier] =
      var mods = SymOps.getExtraModifiers(symbol)()
      if ccEnabled && symbol.flags.is(Flags.Mutable) then
        if symbol.hasAnnotation(cc.CaptureDefs.ConsumeAnnot) then
          mods :+= Modifier.Consume
        else
          mods :+= Modifier.Update
      mods
  }

  private def bareClasslikeKind(using Quotes)(symbol: reflect.Symbol): Kind =
    import reflect._
    if symbol.flags.is(Flags.Module) then Kind.Object
    else if symbol.flags.is(Flags.Trait) then  Kind.Trait(Nil, Nil)
    else if symbol.flags.is(Flags.Enum) then Kind.Enum(Nil, Nil)
    else if symbol.flags.is(Flags.Enum) && symbol.flags.is(Flags.Case) then Kind.EnumCase(Kind.Object)
    else Kind.Class(Nil, Nil)

  private def kindForClasslike(classDef: ClassDef): Kind =
    def typeArgs = classDef.getTypeParams.map(mkTypeArgument(_, classDef))

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

      constr.fold(Nil)(_.termParamss.map(pList =>
        api.TermParameterList(
          pList.params.map(p => mkParameter(p, classDef, parameterModifier)),
          paramListModifier(pList.params),
        )
      ))

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
    def unpackTreeToClassDef(tree: Tree): ClassDef =
      def unpackApply(a: Apply) =
        a.symbol.owner.tree match
          case tree: ClassDef => tree

      tree match
        case tree: ClassDef => tree
        case TypeDef(_, tbt: TypeBoundsTree) => unpackTreeToClassDef(tbt.tpe.typeSymbol.tree)
        case TypeDef(_, tt: TypeTree) => unpackTreeToClassDef(tt.tpe.typeSymbol.tree)
        case c: Apply => unpackApply(c)
        case Block(_, c: Apply) => unpackApply(c)
        case tt: TypeTree => unpackTreeToClassDef(tt.tpe.typeSymbol.tree)

    def signatureWithName(s: dotty.tools.scaladoc.Signature): dotty.tools.scaladoc.Signature =
      s match
          case dotty.tools.scaladoc.Type(n, Some(dri)) :: tail => dotty.tools.scaladoc.Name(n, dri) :: tail
          case other => other

    def getSupertypesGraph(link: LinkToType, to: Seq[Tree]): Seq[(LinkToType, LinkToType)] =
      to.flatMap { case tree =>
        val symbol = if tree.symbol.isClassConstructor then tree.symbol.owner else tree.symbol
        val signature = signatureWithName(tree.asSignature(classDef, classDef.symbol))
        val superLink = LinkToType(signature, symbol.dri, bareClasslikeKind(symbol))
        val nextTo = unpackTreeToClassDef(tree).parents
        if symbol.isHiddenByVisibility then getSupertypesGraph(link, nextTo)
        else Seq(link -> superLink) ++ getSupertypesGraph(superLink, nextTo)
      }

    val supertypes = getSupertypes(using qctx)(classDef)
      .filterNot((s, t) => s.isHiddenByVisibility)
      .map {
        case (symbol, tpe) =>
          val signature = signatureWithName(tpe.asSignature(classDef, classDef.symbol))
          LinkToType(signature, symbol.dri, bareClasslikeKind(symbol))
      }
    val selfType = classDef.self.map { (valdef: ValDef) =>
      val symbol = valdef.symbol
      val tpe = valdef.tpt.tpe
      val owner = if symbol.exists then symbol.owner else Symbol.noSymbol
      val signature = signatureWithName(tpe.asSignature(classDef, owner))
      LinkToType(signature, symbol.dri, Kind.Type(false, false, Seq.empty))
    }
    val selfSignature: DSignature = signatureWithName(typeForClass(classDef).asSignature(classDef, classDef.symbol))

    val graph = HierarchyGraph.withEdges(
      getSupertypesGraph(LinkToType(selfSignature, classDef.symbol.dri, bareClasslikeKind(classDef.symbol)), unpackTreeToClassDef(classDef).parents)
    )

    val kind = if intrinsicClassDefs.contains(classDef.symbol) then Kind.Class(Nil, Nil) else kindForClasslike(classDef)

    val baseMember = mkMember(classDef.symbol, kind, selfSignature)(
      modifiers = modifiers,
      graph = graph,
      deprecated = classDef.symbol.isDeprecated(),
      experimental = classDef.symbol.isExperimental()
    ).copy(
      directParents = classDef.getParentsAsLinkToTypes,
      parents = supertypes,
    )

    if summon[DocContext].args.generateInkuire then doInkuireStuff(classDef)

    if signatureOnly then baseMember else baseMember.copy(
        members = classDef.extractMembers.sortBy(m => (m.name, m.kind.name)),
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

  private def isEvidence(tpc: TermParamClause) =
    (tpc.isGiven || tpc.isImplicit) && tpc.params.forall(_.name.startsWith(NameKinds.ContextBoundParamName.separator))

  private def extractEvidences(tpcs: List[TermParamClause]): (Map[Symbol, List[TypeRepr]], List[TermParamClause]) =
    val (evidenceParams, termParams) = tpcs.partition(isEvidence)
    val evidenceMap = evidenceParams.flatMap(_.params).map(p => (p.tpt, p.tpt.tpe)).collect {
      case (Applied(bound, List(arg: TypeTree)), _) => (arg.tpe.typeSymbol, bound.tpe)
      case (_, AppliedType(bound, List(arg)))       => (arg.typeSymbol, bound)
      // It seems like here we could do:
      //   (...).map(_.tpt.tpe).collect {
      //     case AppliedType(bound, List(arg)) => (arg.typeSymbol, bound)
      // or:
      //   (...).map(_.tpt).collect {
      //     case Applied(bound, List(arg: TypeTree)) => (arg.tpe.typeSymbol, bound.tpe)
      //
      // First one doesn't always work because .tpe in some cases causes type lambda reductions, eg:
      //   def foo[T : ([X] =>> String)]
      // after desugaring:
      //   def foo[T](implicit ecidence$1 : ([X] =>> String)[T])
      // tree for this evidence looks like: ([X] =>> String)[T]
      // but type repr looks like: String
      // (see scaladoc-testcases/src/tests/contextBounds.scala)
      //
      // Second one doesn't always work, because the tree is sometimes `Inferred`
      // (see toArray inherited in scaladoc-testcases/src/tests/classSignatureTestSource.scala)
      //
      // TODO: check if those two cases can occur at the same time
    }.groupMap(_._1)(_._2).withDefaultValue(Nil)
    (evidenceMap, termParams)

  private def parseMember(c: ClassDef)(s: Tree): Option[Member] = processTreeOpt(s) { s match
      case dd: DefDef if isDocumentableExtension(dd.symbol) =>
        dd.symbol.extendedSymbol.map { extSym =>
          val (evidenceMap, termParamClauses) = extractEvidences(dd.symbol.extendedTermParamLists)
          val termParams = termParamClauses.map: tpc =>
            api.TermParameterList(tpc.params.map(mkParameter(_, c)), paramListModifier(tpc.params))
          val typeParams = dd.symbol.extendedTypeParams.map(td => mkTypeArgument(td, c, evidenceMap(td.symbol)))

          val target = ExtensionTarget(
            extSym.symbol.normalizedName,
            typeParams,
            termParams,
            extSym.tpt.asSignature(c, extSym.symbol.owner),
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
        Some(parseTypeDef(td, c))

      case vd: ValDef if !isSyntheticField(vd.symbol) && (!vd.symbol.flags.is(Flags.Case) || !vd.symbol.flags.is(Flags.Enum)) =>
        Some(parseValDef(c, vd))

      case c: ClassDef if c.symbol.shouldDocumentClasslike =>
        Some(parseClasslike(c))

      case _ => None
  }

  private def parseInheritedMember(c: ClassDef)(s: Tree): Option[Member] =
    def inheritance = Some(InheritedFrom(s.symbol.owner.normalizedName, s.symbol.dri, s.symbol.owner.isHiddenByVisibility))
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
        case dd: DefDef if !dd.symbol.isClassConstructor && !(dd.symbol.isSuperAccessor || dd.symbol.isDefaultHelperMethod) => dd
        case other => other
      }
      c.membersToDocument.flatMap(parseMember(c)) ++
        inherited.flatMap(s => parseInheritedMember(c)(s))
    }

    def getTreeOfFirstParent: Option[Tree] =
      c.getParentsAsTreeSymbolTuples.headOption.map(_._1)

    def getParentsAsLinkToTypes: List[LinkToType] =
      c.getParentsAsTreeSymbolTuples.map {
        (tree, symbol) => LinkToType(tree.asSignature(c, c.symbol, skipThisTypePrefix = true), symbol.dri, bareClasslikeKind(symbol))
      }

    def getParentsAsTreeSymbolTuples: List[(Tree, Symbol)] =
      if noPosClassDefs.contains(c.symbol) then Nil
      else for
        // TODO: add exists function to position methods in Quotes and replace the condition here for checking the JPath
        parentTree <- c.parents if parentTree.pos.sourceFile.getJPath.isDefined && parentTree.pos.start != parentTree.pos.end // We assume here that order is correct
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

    def getCompanion: Option[(Kind, DRI)] = c.symbol.getCompanionSymbol
      .filter(!_.flags.is(Flags.Synthetic))
      .filterNot(_.isHiddenByVisibility)
      .map(s => (bareClasslikeKind(s), s.dri))


  def parseClasslike(classDef: ClassDef, signatureOnly: Boolean = false): Member = classDef match
    case c: ClassDef if classDef.symbol.flags.is(Flags.Module) => parseObject(c, signatureOnly)
    case c: ClassDef if classDef.symbol.flags.is(Flags.Enum) && !classDef.symbol.flags.is(Flags.Case) => parseEnum(c, signatureOnly)
    case clazz => mkClass(classDef)(signatureOnly = signatureOnly)

  def parseObject(classDef: ClassDef, signatureOnly: Boolean = false): Member =
    mkClass(classDef)(
      // All objects are final so we do not need final modifier!
      modifiers = classDef.symbol.getExtraModifiers().filter(mod => mod != Modifier.Final && mod != Modifier.Opaque),
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
    }.toList.map(parseTypeDef(_, classDef))

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
      paramPrefix: Symbol => String = _ => "",
      specificKind: (Kind.Def => Kind) = identity
    ): Member =
    val method = methodSymbol.tree.asInstanceOf[DefDef]
    val paramLists = methodSymbol.nonExtensionParamLists.filter:
      case TypeParamClause(_) => true
      case tpc@TermParamClause(_) => !isEvidence(tpc)

    val evidenceMap = extractEvidences(method.termParamss)._1

    val basicDefKind: Kind.Def = Kind.Def(paramLists.map:
      case TermParamClause(vds) =>
        Left(api.TermParameterList(
          vds.map(mkParameter(_, c, paramPrefix)),
          paramListModifier(vds)
        ))
      case TypeParamClause(genericTypeList) =>
        Right(genericTypeList.map(td => mkTypeArgument(td, c, evidenceMap(td.symbol))))
    )

    val methodKind =
      if methodSymbol.isClassConstructor then Kind.Constructor(basicDefKind)
      else if methodSymbol.flags.is(Flags.Implicit) then
        val termParamLists: List[TermParamClause] = methodSymbol.nonExtensionTermParamLists
        extractImplicitConversion(method.returnTpt.tpe) match
          case Some(conversion) if termParamLists.size == 0 || (termParamLists.size == 1 && termParamLists.head.params.size == 0) =>
            Kind.Implicit(basicDefKind, Some(conversion))
          case None if termParamLists.size == 1 && termParamLists(0).params.size == 1 =>
            Kind.Implicit(basicDefKind, Some(
              ImplicitConversion(
                termParamLists(0).params(0).tpt.tpe.typeSymbol.dri,
                method.returnTpt.tpe.typeSymbol.dri
              )
            ))
          case _ =>
            Kind.Implicit(basicDefKind, None)
      else if methodSymbol.flags.is(Flags.Given) then Kind.Given(basicDefKind, Some(method.returnTpt.tpe.asSignature(c, methodSymbol.owner)), extractImplicitConversion(method.returnTpt.tpe))
      else specificKind(basicDefKind)

    val origin = if !methodSymbol.isOverridden then Origin.RegularlyDefined else
      val overriddenSyms = methodSymbol.allOverriddenSymbols.map(_.owner)
      Origin.Overrides(overriddenSyms.map(s => Overridden(s.name, s.dri)).toSeq)

    val modifiers = methodKind match
      case _: Kind.Given => methodSymbol
        .getExtraModifiers()
        .filterNot(m => m == Modifier.Lazy || m == Modifier.Final)
      case _ => methodSymbol.getExtraModifiers()

    mkMember(
      methodSymbol,
      methodKind,
      method.returnTpt.tpe.asSignature(c, methodSymbol),
    )(
      modifiers = modifiers,
      origin = origin,
      deprecated = methodSymbol.isDeprecated(),
      experimental = methodSymbol.isExperimental()
    )

  def mkParameter(
    argument: ValDef,
    classDef: ClassDef,
    prefix: Symbol => String = _ => "",
    isExtendedSymbol: Boolean = false,
    isGrouped: Boolean = false,
  ) =
    val symbol = argument.symbol
    val inlinePrefix = if symbol.flags.is(Flags.Inline) then "inline " else ""
    val comsumePrefix = if self.ccEnabled && symbol.hasAnnotation(cc.CaptureDefs.ConsumeAnnot) then "consume " else ""
    val name = symbol.normalizedName
    val nameIfNotSynthetic = Option.when(!symbol.flags.is(Flags.Synthetic))(name)
    val defaultValue = Option.when(symbol.flags.is(Flags.HasDefault))(Plain(" = ..."))
    api.TermParameter(
      symbol.getAnnotations(),
      comsumePrefix + inlinePrefix + prefix(symbol),
      nameIfNotSynthetic,
      symbol.dri,
      argument.tpt.asSignature(classDef, symbol.owner) :++ defaultValue,
      isExtendedSymbol = isExtendedSymbol,
      isGrouped = isGrouped
    )

  def mkTypeArgument(
    argument: TypeDef,
    classDef: ClassDef,
    contextBounds: List[TypeRepr] = Nil,
  ): TypeParameter =
    val symbol = argument.symbol
    val variancePrefix: "+" | "-" | "" =
      if symbol.flags.is(Flags.Covariant) then "+"
      else if symbol.flags.is(Flags.Contravariant) then "-"
      else ""

    val name = symbol.normalizedName
    val isCaptureVar = ccEnabled && argument.derivesFromCapSet

    val normalizedName = if name.matches("_\\$\\d*") then "_" else name
    val boundsSignature = argument.rhs.asSignature(classDef, symbol.owner)
    val signature = boundsSignature ++ contextBounds.flatMap(tr =>
      val wrap = tr match
        case _: TypeLambda => true
        case _ => false
      Plain(" : ") +: inParens(tr.asSignature(classDef, symbol.owner), wrap)
    )

    TypeParameter(
      symbol.getAnnotations(),
      variancePrefix,
      normalizedName,
      symbol.dri,
      signature,
      isCaptureVar,
    )

  def parseTypeDef(typeDef: TypeDef, classDef: ClassDef): Member =
    val symbol = typeDef.symbol
    def isTreeAbstract(typ: Tree): Boolean = typ match {
      case TypeBoundsTree(_, _) => true
      case LambdaTypeTree(params, body) => isTreeAbstract(body)
      case _ => false
    }

    // Detect capture-set type members (type Cap^), which are represented as
    // type Cap >: CapSet <: CapSet^{...} in the compiler.
    val isCaptureVar = ccEnabled && typeDef.derivesFromCapSet

    val (generics, tpeTree) = typeDef.rhs match
      case LambdaTypeTree(params, body) => (params.map(mkTypeArgument(_, classDef)), body)
      case tpe => (Nil, tpe)

    val defaultKind = Kind.Type(!isTreeAbstract(typeDef.rhs), symbol.isOpaque, generics).asInstanceOf[Kind.Type]
    val kind = if symbol.flags.is(Flags.Enum) then Kind.EnumCase(defaultKind)
      else defaultKind

    // For capset members, prepend ^ to the signature (the bounds rendering
    // already elides the CapSet lower/upper defaults, so we just need the caret).
    val sig = tpeTree.asSignature(classDef, symbol.owner)
    val sigWithCaret = if isCaptureVar then Plain("^") :: sig else sig

    if symbol.flags.is(Flags.Exported)
    then {
      val origin = Some(tpeTree).flatMap {
        case TypeBoundsTree(l: TypeTree, h: TypeTree) if l.tpe == h.tpe =>
          Some(Link(l.tpe.typeSymbol.owner.name, l.tpe.typeSymbol.owner.dri))
        case _ => None
      }
      mkMember(symbol, Kind.Exported(kind), sigWithCaret)(
        deprecated = symbol.isDeprecated(),
        origin = Origin.ExportedFrom(origin),
        experimental = symbol.isExperimental()
      )
    }
    else mkMember(symbol, kind, sigWithCaret)(deprecated = symbol.isDeprecated())

  def parseValDef(c: ClassDef, valDef: ValDef): Member =
    val symbol = valDef.symbol
    def defaultKind = if symbol.flags.is(Flags.Mutable) then Kind.Var else Kind.Val
    val sig = valDef.tpt.tpe.asSignature(c, symbol.owner)
    val kind = if symbol.flags.is(Flags.Implicit) then Kind.Implicit(Kind.Val, extractImplicitConversion(valDef.tpt.tpe))
      else if symbol.flags.is(Flags.Given) then Kind.Given(Kind.Val, Some(sig), extractImplicitConversion(valDef.tpt.tpe))
      else if symbol.flags.is(Flags.Enum) then Kind.EnumCase(Kind.Val)
      else defaultKind

    val modifiers = kind match
      case _: Kind.Given => symbol
        .getExtraModifiers()
        .filterNot(m => m == Modifier.Lazy || m == Modifier.Final)
      case _ => symbol.getExtraModifiers()

    mkMember(symbol, kind, sig)(
      // Due to how capture checking encodes update methods (recycling the mutable flag for methods),
      // we need to filter out the update modifier here. Otherwise, mutable fields will
      // be documented as having the update modifier, which is not correct.
      modifiers = modifiers.filterNot(_ == Modifier.Update),
      deprecated = symbol.isDeprecated(),
      experimental = symbol.isExperimental()
    )

  def mkMember(symbol: Symbol, kind: Kind, signature: DSignature)(
    modifiers: Seq[Modifier] = symbol.getExtraModifiers(),
    origin: Origin = Origin.RegularlyDefined,
    inheritedFrom: Option[InheritedFrom] = None,
    graph: HierarchyGraph = HierarchyGraph.empty,
    deprecated: Option[Annotation] = None,
    experimental: Option[Annotation] = None
  ) = Member(
    name = symbol.normalizedName,
    fullName = symbol.normalizedFullName,
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
    deprecated = deprecated,
    experimental = experimental
  )

  private def paramListModifier(parameters: Seq[ValDef]): String =
    if parameters.size > 0 then
      if parameters(0).symbol.flags.is(Flags.Given) then "using "
      else if parameters(0).symbol.flags.is(Flags.Implicit) then "implicit "
      else ""
    else ""
