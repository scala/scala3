package dotty.dokka.tasty

import org.jetbrains.dokka.model.{TypeConstructor => DTypeConstructor, _}
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

  private def kindForClasslike(sym: Symbol): Kind =
        if sym.flags.is(Flags.Module) then Kind.Object
        else if sym.flags.is(Flags.Trait) then Kind.Trait
        else if sym.flags.is(Flags.Enum) then Kind.Enum
        else Kind.Class

  object DClass:
    def apply[T >: DClass](classDef: ClassDef)(
      dri: DRI = classDef.symbol.dri,
      name: String = classDef.symbol.normalizedName,
      signatureOnly: Boolean = false,
      modifiers: Seq[Modifier] = classDef.symbol.getExtraModifiers(),
    ): DClass =

      // This Try is here because of problem that code compiles, but at runtime fails claiming
      // java.lang.ClassCastException: class dotty.tools.dotc.ast.Trees$DefDef cannot be cast to class dotty.tools.dotc.ast.Trees$TypeDef (dotty.tools.dotc.ast.Trees$DefDef and dotty.tools.dotc.ast.Trees$TypeDef are in unnamed module of loader 'app')
      // It is probably bug in Tasty
      def hackGetParents(classDef: ClassDef): Option[List[Tree]] = scala.util.Try(classDef.parents).toOption

      def getSupertypesGraph(classDef: ClassDef, link: LinkToType): Seq[(LinkToType, LinkToType)] =
        val smbl = classDef.symbol
        val parents = if smbl.exists then hackGetParents(smbl.tree.asInstanceOf[ClassDef]) else None
        parents.fold(Seq())(_.flatMap { case tree =>
            val symbol = if tree.symbol.isClassConstructor then tree.symbol.owner else tree.symbol
            val superLink = LinkToType(tree.dokkaType.asSignature, symbol.dri, kindForClasslike(symbol))
            Seq(link -> superLink) ++ getSupertypesGraph(tree.asInstanceOf[ClassDef], superLink)
          }
        )

      val supertypes = getSupertypes(using qctx)(classDef).map {
        case (symbol, tpe) => LinkToType(tpe.dokkaType.asSignature, symbol.dri, kindForClasslike(symbol))
      }
      val selfSiangture: DSignature = typeForClass(classDef).dokkaType.asSignature

      val graph = HierarchyGraph.withEdges(getSupertypesGraph(classDef, LinkToType(selfSiangture, classDef.symbol.dri, kindForClasslike(classDef.symbol))))
      val baseExtra = PropertyContainer.Companion.empty()
            .plus(ClasslikeExtension(classDef.getConstructorMethod(), classDef.getCompanion))
            .plus(MemberExtension(
              classDef.symbol.getVisibility(),
              modifiers,
              kindForClasslike( classDef.symbol),
              classDef.symbol.getAnnotations(),
              selfSiangture,
              classDef.symbol.source,
              graph = graph
            ))

      val fullExtra =
        if (signatureOnly) baseExtra
        else
          baseExtra.plus(CompositeMemberExtension(
            classDef.extractPatchedMembers,
            classDef.getParents.map(_.dokkaType.asSignature),
            supertypes,
            Nil))
        end if

      new DClass(
          dri,
          name,
          (if(signatureOnly) Nil else classDef.getConstructors.map(parseMethod(_))).asJava,
          JList(),
          JList(),
          JList(),
          JMap(),
          placeholderVisibility,
          null,
          /*generics =*/classDef.getTypeParams.map(parseTypeArgument).asJava,
          Map.empty.asJava,
          classDef.symbol.documentation.asJava,
          null,
          placeholderModifier,
          ctx.sourceSet.toSet,
          /*isExpectActual =*/ false,
          fullExtra.asInstanceOf[PropertyContainer[DClass]]
      )

  private val conversionSymbol = Symbol.requiredClass("scala.Conversion")

  def extractImplicitConversion(tpe: TypeRepr): Option[ImplicitConversion] =
      if tpe.derivesFrom(conversionSymbol) then tpe.baseType(conversionSymbol) match
        case AppliedType(tpe, List(from: TypeRepr, to: TypeRepr)) =>
          Some(ImplicitConversion(from.typeSymbol.dri, to.typeSymbol.dri))
        case _ =>
          None
      else None

  private def parseMember(s: Tree): Option[Member] = processTreeOpt(s)(s match
      case dd: DefDef if !dd.symbol.isHiddenByVisibility && !dd.symbol.isSyntheticFunc && dd.symbol.isExtensionMethod =>
        dd.symbol.extendedSymbol.map { extSym =>
          val target = ExtensionTarget(
            extSym.symbol.normalizedName,
            extSym.tpt.dokkaType.asSignature,
            extSym.tpt.symbol.dri,
            extSym.symbol.pos.get.start
          )
          parseMethod(dd.symbol, kind = Kind.Extension(target))
        }
      // TODO check given methods?
      case dd: DefDef if !dd.symbol.isHiddenByVisibility && dd.symbol.isGiven =>
        Some(dd.symbol.owner.memberType(dd.name))
          .filterNot(_.exists)
          .map { _ =>
            parseMethod(dd.symbol, kind = Kind.Given(getGivenInstance(dd).map(_.asSignature), None))
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
        Some(parseMethod(dd.symbol, kind = Kind.Exported).withOrigin(Origin.ExportedFrom(s"$instanceName.$functionName", dri)))

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
    val modifiedClasslikeExtension = ClasslikeExtension.getFrom(parsedClasslike).map(_.copy(
        constructor = c.getConstructorMethod(Some(_ => "using "))
      )
    ).get
    parsedClasslike.withNewExtras(
      parsedClasslike.getExtra.plus(modifiedClasslikeExtension)
    ).withKind(
      Kind.Given(parsedClasslike.directParents.headOption, parentTpe.flatMap(extractImplicitConversion))
    )
  }

  private def parseInheritedMember(s: Tree): Option[Member] = processTreeOpt(s)(s match
    case c: ClassDef if c.symbol.shouldDocumentClasslike && !c.symbol.isGiven => Some(parseClasslike(c, signatureOnly = true))
    case other => parseMember(other)
  ).map(_.withOrigin(Origin.InheritedFrom(s.symbol.owner.normalizedName, s.symbol.owner.dri)))

  extension (c: ClassDef):
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
      val fieldSymbol = c.symbol.field(parameter.normalizedName)
      if fieldSymbol.flags.is(Flags.Mutable) then "var "
      else if fieldSymbol.flags.is(Flags.ParamAccessor) && !c.symbol.flags.is(Flags.Case) && !fieldSymbol.flags.is(Flags.Private) then "val "
      else ""

    def getTypeParams: List[TypeDef] = c.body.collect { case targ: TypeDef => targ  }.filter(_.symbol.isTypeParam)

    def getCompanion: Option[DRI] = c.symbol.getCompanionSymbol
      .filter(!_.flags.is(Flags.Synthetic))
      .filterNot(_.isHiddenByVisibility)
      .map(_.dri)

    def getConstructorMethod(paramModifierFunc: Option[Symbol => String] = None): Option[DFunction] =
      Some(c.constructor.symbol).filter(_.exists).filterNot(_.isHiddenByVisibility).map( d =>
        parseMethod(d, constructorWithoutParamLists(c), paramModifierFunc.getOrElse(s => c.getParameterModifier(s)))
      )

  def parseClasslike(classDef: ClassDef, signatureOnly: Boolean = false): DClass = classDef match
    case c: ClassDef if classDef.symbol.flags.is(Flags.Module) => parseObject(c, signatureOnly)
    case c: ClassDef if classDef.symbol.flags.is(Flags.Enum) => parseEnum(c, signatureOnly)
    case clazz => DClass(classDef)(signatureOnly = signatureOnly)

  def parseObject(classDef: ClassDef, signatureOnly: Boolean = false): DClass =
    DClass(classDef)(
      // All objects are final so we do not need final modifer!
      modifiers = classDef.symbol.getExtraModifiers().filter(_ != Modifier.Final),
      signatureOnly = signatureOnly
    )

    // TODO check withNewExtras?
  def parseEnum(classDef: ClassDef, signatureOnly: Boolean = false): DClass =
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

    val classlikie = DClass(classDef)(modifiers = extraModifiers, signatureOnly = signatureOnly)
    classlikie.withNewMembers((enumVals ++ enumTypes ++ enumNested).map(_.withKind(Kind.EnumCase))).asInstanceOf[DClass]

  def parseMethod(
      methodSymbol: Symbol,
      emptyParamsList: Boolean = false,
      paramPrefix: Symbol => String = _ => "",
      kind: Kind = Kind.Def
    ): DFunction =
    val method = methodSymbol.tree.asInstanceOf[DefDef]
    val paramLists = if emptyParamsList then Nil else method.paramss
    val genericTypes = if (methodSymbol.isClassConstructor) Nil else method.typeParams

    val methodKind =
      if methodSymbol.isClassConstructor then Kind.Constructor
      else if methodSymbol.flags.is(Flags.Implicit) then extractImplicitConversion(method.returnTpt.tpe) match
        case Some(conversion) if paramLists.size == 0 || (paramLists.size == 1 && paramLists.head.size == 0) =>
          Kind.Implicit(Kind.Def, Some(conversion))
        case None if paramLists.size == 1 && paramLists(0).size == 1 =>
          Kind.Implicit(Kind.Def, Some(
            ImplicitConversion(
              paramLists(0)(0).tpt.tpe.typeSymbol.dri,
              method.returnTpt.tpe.typeSymbol.dri
            )
          ))
        case _ =>
          Kind.Implicit(Kind.Def, None)
      else kind

    val name = method.symbol.normalizedName

    new DFunction(
      methodSymbol.dri,
      name,
      /*isConstructor =*/ methodSymbol.isClassConstructor,
      /*parameters =*/ paramLists.flatten.map(parseArgument(_, paramPrefix)).asJava, // TODO add support for parameters
      /*documentation =*/ methodSymbol.documentation.asJava,
      /*expectPresentInSet =*/ null, // unused
      /*sources =*/ JMap(),
      /*visibility =*/ placeholderVisibility,
      /*type =*/ method.returnTpt.dokkaType,
      /*generics =*/ genericTypes.map(parseTypeArgument).asJava,
      /*receiver =*/ null, // Not used
      /*modifier =*/ placeholderModifier,
      ctx.sourceSet.toSet,
       /*isExpectActual =*/ false,
      PropertyContainer.Companion.empty()
        plus MethodExtension(paramLists.map(_.size))
        plus(MemberExtension(
          methodSymbol.getVisibility(),
          methodSymbol.getExtraModifiers(),
          methodKind,
          methodSymbol.getAnnotations(),
          method.returnTpt.dokkaType.asSignature,
          methodSymbol.source
        ))
    )

  def parseArgument(argument: ValDef, prefix: Symbol => String, isExtendedSymbol: Boolean = false, isGrouped: Boolean = false): DParameter =
    new DParameter(
      argument.symbol.dri,
      prefix(argument.symbol) + argument.symbol.normalizedName,
      argument.symbol.documentation.asJava,
      null,
      argument.tpt.dokkaType,
      ctx.sourceSet.toSet,
      PropertyContainer.Companion.empty()
        .plus(ParameterExtension(isExtendedSymbol, isGrouped))
        .plus(MemberExtension.empty.copy(annotations = argument.symbol.getAnnotations()))
    )

  def parseTypeArgument(argument: TypeDef): DTypeParameter =
    // Not sure if we should have such hacks...
    val variancePrefix =
      if  argument.symbol.flags.is(Flags.Covariant) then "+"
      else if argument.symbol.flags.is(Flags.Contravariant) then "-"
      else ""

    new DTypeParameter(
      Invariance(TypeParameter(argument.symbol.dri, variancePrefix + argument.symbol.normalizedName, null)),
      argument.symbol.documentation.asJava,
      null,
      JList(argument.rhs.dokkaType),
      ctx.sourceSet.toSet,
      PropertyContainer.Companion.empty()
    )

  def parseTypeDef(typeDef: TypeDef): DProperty =

    def isTreeAbstract(typ: Tree): Boolean = typ match {
      case TypeBoundsTree(_, _) => true
      case LambdaTypeTree(params, body) => isTreeAbstract(body)
      case _ => false
    }


    val (generics, tpeTree) = typeDef.rhs match
      case LambdaTypeTree(params, body) => (params.map(parseTypeArgument), body)
      case tpe => (Nil, tpe)

    new DProperty(
      typeDef.symbol.dri,
      typeDef.symbol.normalizedName,
      /*documentation =*/ typeDef.symbol.documentation.asJava,
      /*expectPresentInSet =*/ null, // unused
      /*sources =*/ JMap(),
      /*visibility =*/ placeholderVisibility,
      /*type =*/ tpeTree.dokkaType, // TODO this may be hard...
      /*receiver =*/ null, // Not used
      /*setter =*/ null,
      /*getter =*/ null,
      /*modifier =*/ placeholderModifier,
      ctx.sourceSet.toSet,
      /*generics =*/ generics.asJava, // TODO
       /*isExpectActual =*/ false,
      PropertyContainer.Companion.empty() plus MemberExtension(
        typeDef.symbol.getVisibility(),
        typeDef.symbol.getExtraModifiers(),
        Kind.Type(!isTreeAbstract(typeDef.rhs), typeDef.symbol.isOpaque),
        typeDef.symbol.getAnnotations(),
        tpeTree.dokkaType.asSignature,
        typeDef.symbol.source
        )
    )

  def parseValDef(valDef: ValDef): DProperty =
    def defaultKind = if valDef.symbol.flags.is(Flags.Mutable) then Kind.Var else Kind.Val
    val kind = if valDef.symbol.flags.is(Flags.Implicit) then
        Kind.Implicit(Kind.Val, extractImplicitConversion(valDef.tpt.tpe))
        else defaultKind

    new DProperty(
      valDef.symbol.dri,
      valDef.symbol.normalizedName,
      /*documentation =*/ valDef.symbol.documentation.asJava,
      /*expectPresentInSet =*/ null, // unused
      /*sources =*/ JMap(),
      /*visibility =*/ placeholderVisibility,
      /*type =*/ valDef.tpt.dokkaType,
      /*receiver =*/ null, // Not used
      /*setter =*/ null,
      /*getter =*/ null,
      /*modifier =*/ placeholderModifier,
      ctx.sourceSet.toSet,
      /*generics =*/ JList(),
       /*isExpectActual =*/ false,
      PropertyContainer.Companion.empty().plus(MemberExtension(
          valDef.symbol.getVisibility(),
          valDef.symbol.getExtraModifiers(),
          kind,
          valDef.symbol.getAnnotations(),
          valDef.tpt.tpe.dokkaType.asSignature,
          valDef.symbol.source
      ))
    )
