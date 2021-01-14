package dotty.dokka.tasty

import org.jetbrains.dokka.model.{TypeConstructor => DTypeConstructor, TypeParameter => _, _}
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties._
import dotty.dokka._
import org.jetbrains.dokka.base.transformers.documentables.CallableExtensions
import dotty.dokka.model.api._
import dotty.dokka.model.api.Modifier
import dotty.dokka.model.api.Kind
import dotty.dokka.model.api.ImplicitConversion
import dotty.dokka.model.api.{Signature => DSignature, Link => DLink}

trait ClassLikeSupport:
  self: TastyParser =>
  import qctx.reflect._

  private val placeholderVisibility = JMap(ctx.sourceSet -> KotlinVisibility.Public.INSTANCE)
  private val placeholderModifier = JMap(ctx.sourceSet -> KotlinModifier.Empty.INSTANCE)

  private def bareClasslikeKind(symbol: Symbol): Kind =
     if symbol.flags.is(Flags.Module) then Kind.Object
        else if symbol.flags.is(Flags.Trait) then  Kind.Trait(Nil, Nil)
        else if symbol.flags.is(Flags.Enum) then Kind.Enum
        else Kind.Class(Nil, Nil)

  private def kindForClasslike(classDef: ClassDef): Kind =
    def typeArgs = classDef.getTypeParams.map(mkTypeArgument)

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
        _.paramss.map(pList => ParametersList(pList.map(mkParameter(_, parameterModifier)), if isUsingModifier(pList) then "using " else ""))
        )

    if classDef.symbol.flags.is(Flags.Module) then Kind.Object
    else if classDef.symbol.flags.is(Flags.Trait) then
      Kind.Trait(typeArgs, args)
    else if classDef.symbol.flags.is(Flags.Enum) then Kind.Enum
    else Kind.Class(typeArgs, args)

  def mkClass[T >: DClass](classDef: ClassDef)(
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
        val superLink = LinkToType(tree.dokkaType.asSignature, symbol.dri, bareClasslikeKind(symbol))
        Seq(link -> superLink) ++ getSupertypesGraph(tree, superLink)
      }

    val supertypes = getSupertypes(using qctx)(classDef).map {
      case (symbol, tpe) =>
        LinkToType(tpe.dokkaType.asSignature, symbol.dri, bareClasslikeKind(symbol))
    }
    val selfSiangture: DSignature = typeForClass(classDef).dokkaType.asSignature

    val graph = HierarchyGraph.withEdges(getSupertypesGraph(classDef,
      LinkToType(selfSiangture, classDef.symbol.dri, bareClasslikeKind(classDef.symbol))))

    val compositeExt =
      if signatureOnly then CompositeMemberExtension.empty
      else CompositeMemberExtension(
          classDef.extractPatchedMembers,
          classDef.getParents.map(_.dokkaType.asSignature),
          supertypes,
          Nil,
          classDef.getCompanion
        )

    mkMember(
      classDef.symbol,
      MemberExtension(
          classDef.symbol.getVisibility(),
          modifiers,
          kindForClasslike(classDef),
          classDef.symbol.getAnnotations(),
          selfSiangture,
          classDef.symbol.source(using qctx),
          graph = graph
        ),
        compositeExt
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

  private def parseMember(s: Tree): Option[Member] = processTreeOpt(s)(s match
      case dd: DefDef if isDocumentableExtension(dd.symbol) =>
        dd.symbol.extendedSymbol.map { extSym =>
          val target = ExtensionTarget(
            extSym.symbol.normalizedName,
            extSym.tpt.dokkaType.asSignature,
            extSym.tpt.symbol.dri,
            extSym.symbol.pos.get.start
          )
          parseMethod(dd.symbol,specificKind = Kind.Extension(target, _))
        }
      // TODO check given methods?
      case dd: DefDef if !dd.symbol.isHiddenByVisibility && dd.symbol.isGiven =>
        Some(dd.symbol.owner.memberType(dd.name))
          .filterNot(_.exists)
          .map { _ =>
            parseMethod(dd.symbol, specificKind =
              Kind.Given(_, getGivenInstance(dd).map(_.asSignature), None)
            )
          }

      case dd: DefDef if !dd.symbol.isHiddenByVisibility && dd.symbol.isExported =>
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

        Some(parseMethod(dd.symbol, specificKind = Kind.Exported(_))
          .withOrigin(Origin.ExportedFrom(s"$instanceName.$functionName", dri)))

      case dd: DefDef if !dd.symbol.isHiddenByVisibility && !dd.symbol.isGiven && !dd.symbol.isSyntheticFunc && !dd.symbol.isExtensionMethod =>
        Some(parseMethod(dd.symbol))

      case td: TypeDef if !td.symbol.flags.is(Flags.Synthetic) && (!td.symbol.flags.is(Flags.Case) || !td.symbol.flags.is(Flags.Enum)) =>
        Some(parseTypeDef(td))

      case vd: ValDef if !isSyntheticField(vd.symbol)
        && (!vd.symbol.flags.is(Flags.Case) || !vd.symbol.flags.is(Flags.Enum))
        && vd.symbol.isGiven =>
          val classDef = Some(vd.tpt.tpe).flatMap(_.classSymbol.map(_.tree.asInstanceOf[ClassDef]))
          Some(classDef.filter(_.symbol.flags.is(Flags.Module)).fold[Member](parseValDef(vd))(parseGivenClasslike(_)))

      case vd: ValDef if !isSyntheticField(vd.symbol) && (!vd.symbol.flags.is(Flags.Case) || !vd.symbol.flags.is(Flags.Enum)) =>
        Some(parseValDef(vd))

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
      Kind.Given(cls, givenParents, parentTpe.flatMap(extractImplicitConversion))
    )
  }

  private def parseInheritedMember(s: Tree): Option[Member] = processTreeOpt(s)(s match
    case c: ClassDef if c.symbol.shouldDocumentClasslike && !c.symbol.isGiven => Some(parseClasslike(c, signatureOnly = true))
    case other => parseMember(other)
  ).map(_.withInheritedFrom(InheritedFrom(s.symbol.owner.normalizedName, s.symbol.owner.dri)))

  extension (c: ClassDef)
    def membersToDocument = c.body.filterNot(_.symbol.isHiddenByVisibility)

    def getNonTrivialInheritedMemberTrees =
      c.symbol.getAllMembers.filterNot(s => s.isHiddenByVisibility || s.maybeOwner == c.symbol)
        .filter(s => s.maybeOwner != defn.ObjectClass && s.maybeOwner != defn.AnyClass)
        .map(_.tree)

    def extractMembers: Seq[Member] = {
      val inherited = c.getNonTrivialInheritedMemberTrees.collect {
        case dd: DefDef if !dd.symbol.isClassConstructor && !(dd.symbol.isSuperBridgeMethod || dd.symbol.isDefaultHelperMethod) => dd
        case other => other
      }
      c.membersToDocument.flatMap(parseMember) ++
        inherited.flatMap(s => parseInheritedMember(s))
    }

    /** Extracts members while taking Dotty logic for patching the stdlib into account. */
    def extractPatchedMembers: Seq[Member] = {
      val ownMembers = c.extractMembers
      def extractPatchMembers(sym: Symbol) = {
        // NOTE for some reason scala.language$.experimental$ class doesn't show up here, so we manually add the name
        val ownMemberDRIs = ownMembers.iterator.map(_.name).toSet + "experimental$"
        sym.tree.asInstanceOf[ClassDef]
          .membersToDocument.filterNot(m => ownMemberDRIs.contains(m.symbol.name))
          .flatMap(parseMember)
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

    def getParents: List[Tree] =
      for
        parentTree <- c.parents if isValidPos(parentTree.pos)  // We assume here that order is correct
        parentSymbol = if parentTree.symbol.isClassConstructor then parentTree.symbol.owner else parentTree.symbol
        if parentSymbol != defn.ObjectClass && parentSymbol != defn.AnyClass
      yield parentTree


    def getConstructors: List[Symbol] = membersToDocument.collect {
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
    case c: ClassDef if classDef.symbol.flags.is(Flags.Enum) => parseEnum(c, signatureOnly)
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
    }.toList.map(parseValDef(_))

    val enumTypes = companion.membersToDocument.collect {
      case td: TypeDef if !td.symbol.flags.is(Flags.Synthetic) && td.symbol.flags.is(Flags.Enum) && td.symbol.flags.is(Flags.Case) => td
    }.toList.map(parseTypeDef)

    val enumNested = companion.membersToDocument.collect {
      case c: ClassDef if c.symbol.flags.is(Flags.Case) && c.symbol.flags.is(Flags.Enum) => processTree(c)(parseClasslike(c))
    }.flatten

    val classlikie = mkClass(classDef)(modifiers = extraModifiers, signatureOnly = signatureOnly)
    val cases =
      enumNested.map(_.withKind(Kind.EnumCase(Kind.Object))) ++
      enumTypes.map(et => et.withKind(Kind.EnumCase(et.kind.asInstanceOf[Kind.Type]))) ++
      enumVals.map(_.withKind(Kind.EnumCase(Kind.Val)))

    classlikie.withNewMembers(cases).asInstanceOf[DClass]

  def parseMethod(
      methodSymbol: Symbol,
      emptyParamsList: Boolean = false,
      paramPrefix: Symbol => String = _ => "",
      specificKind: (Kind.Def => Kind) = identity
    ): Member =
    val method = methodSymbol.tree.asInstanceOf[DefDef]
    val paramLists =
      if emptyParamsList then Nil
      else if methodSymbol.isExtensionMethod then
        val params = method.paramss
        if methodSymbol.isLeftAssoc || params.size == 1 then params.tail
        else params.head :: params.tail.drop(1)
      else method.paramss
    val genericTypes = if (methodSymbol.isClassConstructor) Nil else method.typeParams

    val basicKind: Kind.Def = Kind.Def(
      genericTypes.map(mkTypeArgument),
      paramLists.map(pList => ParametersList(pList.map(mkParameter(_, paramPrefix)), if isUsingModifier(pList) then "using " else ""))
    )

    val methodKind =
      if methodSymbol.isClassConstructor then Kind.Constructor(basicKind)
      else if methodSymbol.flags.is(Flags.Implicit) then extractImplicitConversion(method.returnTpt.tpe) match
        case Some(conversion) if paramLists.size == 0 || (paramLists.size == 1 && paramLists.head.size == 0) =>
          Kind.Implicit(basicKind, Some(conversion))
        case None if paramLists.size == 1 && paramLists(0).size == 1 =>
          Kind.Implicit(basicKind, Some(
            ImplicitConversion(
              paramLists(0)(0).tpt.tpe.typeSymbol.dri,
              method.returnTpt.tpe.typeSymbol.dri
            )
          ))
        case _ =>
          Kind.Implicit(basicKind, None)
      else specificKind(basicKind)

    val origin = if !methodSymbol.isOverriden then Origin.RegularlyDefined else
      val overridenSyms = methodSymbol.allOverriddenSymbols.map(_.owner)
      Origin.Overrides(overridenSyms.map(s => Overriden(s.name, s.dri)).toSeq)

    mkMember(
      method.symbol,
      MemberExtension(
        methodSymbol.getVisibility(),
        methodSymbol.getExtraModifiers(),
        methodKind,
        methodSymbol.getAnnotations(),
        method.returnTpt.dokkaType.asSignature,
        methodSymbol.source(using qctx),
        origin
      )
    )

  def mkParameter(argument: ValDef,
    prefix: Symbol => String = _ => "",
    isExtendedSymbol: Boolean = false,
    isGrouped: Boolean = false) =
      val inlinePrefix = if argument.symbol.flags.is(Flags.Inline) then "inline " else ""
      val name = Option.when(!argument.symbol.flags.is(Flags.Synthetic))(argument.symbol.normalizedName)

      Parameter(
        argument.symbol.getAnnotations(),
        inlinePrefix + prefix(argument.symbol),
        name,
        argument.symbol.dri,
        argument.tpt.dokkaType.asSignature,
        isExtendedSymbol,
        isGrouped
      )

  def mkTypeArgument(argument: TypeDef): TypeParameter =
    val variancePrefix: "+" | "-" | "" =
      if  argument.symbol.flags.is(Flags.Covariant) then "+"
      else if argument.symbol.flags.is(Flags.Contravariant) then "-"
      else ""

    TypeParameter(
      argument.symbol.getAnnotations(),
      variancePrefix,
      argument.symbol.normalizedName,
      argument.symbol.dri,
      argument.rhs.dokkaType.asSignature
    )

  def parseTypeDef(typeDef: TypeDef): Member =
    def isTreeAbstract(typ: Tree): Boolean = typ match {
      case TypeBoundsTree(_, _) => true
      case LambdaTypeTree(params, body) => isTreeAbstract(body)
      case _ => false
    }

    val (generics, tpeTree) = typeDef.rhs match
      case LambdaTypeTree(params, body) => (params.map(mkTypeArgument), body)
      case tpe => (Nil, tpe)

    mkMember(
      typeDef.symbol,
      MemberExtension(
        typeDef.symbol.getVisibility(),
        typeDef.symbol.getExtraModifiers(),
        Kind.Type(!isTreeAbstract(typeDef.rhs), typeDef.symbol.isOpaque, generics),
        typeDef.symbol.getAnnotations(),
        tpeTree.dokkaType.asSignature,
        typeDef.symbol.source(using qctx)
        )
    )

  def parseValDef(valDef: ValDef): Member =
    def defaultKind = if valDef.symbol.flags.is(Flags.Mutable) then Kind.Var else Kind.Val
    val kind = if valDef.symbol.flags.is(Flags.Implicit) then
        Kind.Implicit(Kind.Val, extractImplicitConversion(valDef.tpt.tpe))
        else defaultKind

    mkMember(
      valDef.symbol,
      MemberExtension(
          valDef.symbol.getVisibility(),
          valDef.symbol.getExtraModifiers(),
          kind,
          valDef.symbol.getAnnotations(),
          valDef.tpt.tpe.dokkaType.asSignature,
          valDef.symbol.source(using qctx)
      )
    )

  def mkMember[T <: Kind](
    symbol: Symbol,
    member: MemberExtension,
    compositeExt: CompositeMemberExtension = CompositeMemberExtension.empty,
    nameOverride: Symbol => String = _.normalizedName
    ): Member =
      new DClass(
        symbol.dri,
        nameOverride(symbol),
        JNil,
        JNil,
        JNil,
        JNil,
        emptyJMap,
        placeholderVisibility,
        null,
        JNil,
        emptyJMap,
        emptyJMap,
        null,
        placeholderModifier,
        ctx.sourceSet.toSet,
        /*isExpectActual =*/ false,
        PropertyContainer.Companion.empty().plus(member.copy(rawDoc = symbol.documentation)).plus(compositeExt)
    )

  private def isUsingModifier(parameters: Seq[ValDef]): Boolean =
    parameters.size > 0 && parameters(0).symbol.flags.is(Flags.Given)
