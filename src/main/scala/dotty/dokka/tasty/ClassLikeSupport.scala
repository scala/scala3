package dotty.dokka.tasty

import org.jetbrains.dokka.model.{TypeConstructor => DTypeConstructor, _}
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.DokkaConfiguration$DokkaSourceSet
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties._
import dotty.dokka._
import org.jetbrains.dokka.base.transformers.documentables.CallableExtensions

trait ClassLikeSupport:
  self: TastyParser =>
  import reflect._

  object DClass:
    def apply[T >: DClass](classDef: ClassDef)(
      kind: Kind,
      dri: DRI = classDef.symbol.dri,
      name: String = classDef.name,
      constructors: List[DFunction] = classDef.getConstructors.map(parseMethod(_)),
      methods: List[DFunction] = classDef.getMethods.map(parseMethod(_)),
      fields: List[DProperty] = (classDef.getTypeDefs.map(parseTypeDef) ++ classDef.getValDefs.map(parseValDef(_))),
      nested: List[DClasslike] = classDef.getNestedClasslikes,
      sources: Map[DokkaConfiguration$DokkaSourceSet, DocumentableSource] = classDef.symbol.source,
      visibility: Map[DokkaConfiguration$DokkaSourceSet, Visibility] = Map(sourceSet.getSourceSet -> (classDef.symbol.getVisibility())),
      generics: List[DTypeParameter] = classDef.getTypeParams.map(parseTypeArgument),
      supertypes: Map[DokkaConfiguration$DokkaSourceSet, List[TypeConstructorWithKind]] = Map.empty,
      documentation: Map[DokkaConfiguration$DokkaSourceSet, DocumentationNode] = classDef.symbol.documentation,
      modifier: Map[DokkaConfiguration$DokkaSourceSet, Modifier] = Map(sourceSet.getSourceSet -> classDef.symbol.getModifier()),
      additionalExtras: Seq[ExtraProperty[DClass]] = Seq.empty
    ): DClass = {
      val annotations = AnnotationsInfo(classDef.symbol.getAnnotations())
      new DClass(
          dri,
          name,
          constructors.asJava,
          methods.asJava,
          fields.asJava,
          nested.asJava,
          sources.asJava,
          visibility.asJava,
          null,
          generics.asJava,
          supertypes.map{case (key,value) => (key, value.asJava)}.asJava,
          documentation.asJava,
          null,
          modifier.asJava,
          inspector.sourceSet.toSet,
          PropertyContainer.Companion.empty()
            .plus(ClasslikeExtension(
              classDef.getParents,
              classDef.getConstructorMethod,
              kind,
              classDef.getCompanion,
              classDef.getExtensionGroups,
              classDef.getInheritedDefinitions,
              classDef.getGivenMethods ++ classDef.getGivenFields
            ))
            .plus(AdditionalModifiers(sourceSet.asMap(classDef.symbol.getExtraModifiers().asJava)))
            .plus(InheritanceInfo(classDef.getSupertypes, List.empty))
            .plus(annotations)
            .plus(ImplicitConversions(classDef.getImplicitConversions))
            .addAll(additionalExtras.asJava)
      )
    }

    def parseForSignatureOnly(classDef: ClassDef)(
      kind: Kind,
      dri: DRI = classDef.symbol.dri,
      name: String = classDef.name,
      constructors: List[DFunction] = classDef.getConstructors.map(parseMethod(_)),
      sources: Map[DokkaConfiguration$DokkaSourceSet, DocumentableSource] = classDef.symbol.source,
      visibility: Map[DokkaConfiguration$DokkaSourceSet, Visibility] = Map(sourceSet.getSourceSet -> (classDef.symbol.getVisibility())),
      generics: List[DTypeParameter] = classDef.getTypeParams.map(parseTypeArgument),
      supertypes: Map[DokkaConfiguration$DokkaSourceSet, List[TypeConstructorWithKind]] = Map.empty,
      documentation: Map[DokkaConfiguration$DokkaSourceSet, DocumentationNode] = classDef.symbol.documentation,
      modifier: Map[DokkaConfiguration$DokkaSourceSet, Modifier] = Map(sourceSet.getSourceSet -> classDef.symbol.getModifier()),
      additionalExtras: Seq[ExtraProperty[DClass]] = Seq.empty
    ): DClass = {
      val annotations = AnnotationsInfo(classDef.symbol.getAnnotations())
      new DClass(
          dri,
          name,
          JList(),
          JList(),
          JList(),
          JList(),
          sources.asJava,
          visibility.asJava,
          null,
          generics.asJava,
          supertypes.map{case (key,value) => (key, value.asJava)}.asJava,
          documentation.asJava,
          null,
          modifier.asJava,
          inspector.sourceSet.toSet,
          PropertyContainer.Companion.empty()
            .plus(ClasslikeExtension(
              classDef.getParents,
              classDef.getConstructorMethod,
              kind,
              None,
              List.empty,
              null,
              List.empty
            ))
            .plus(AdditionalModifiers(sourceSet.asMap(classDef.symbol.getExtraModifiers().asJava)))
            .plus(annotations)
            .addAll(additionalExtras.asJava)
      )
    }

  extension (c: ClassDef):
    def membersToDocument = c.body.filterNot(_.symbol.isHiddenByVisibility)

    def inheritedMembers = c.symbol.getAllMembers.filterNot(s => s.isHiddenByVisibility || s.maybeOwner == c.symbol)

    def getNonTrivialInheritedMemberTrees = c.inheritedMembers
        .filter(s => s.maybeOwner != defn.ObjectClass && s.maybeOwner != defn.AnyClass)
        .map(_.tree)

    private def extractExtensionGroups(functions: List[Symbol]) = {
      case class ExtensionRepr(arg: ValDef, method: Symbol)
      val extensions = functions
        .filterNot(_.isHiddenByVisibility)
        .filterNot(_.isSyntheticFunc)
        .filter(_.isExtensionMethod)
        .map(m => ExtensionRepr(m.extendedSymbol.get, m))
      val groupped = extensions.groupBy( e => e.arg.pos)
      groupped.map {
        case (pos, extensions) => {
          val isGroupped = extensions.size > 1
          val dMethods = extensions.map( (arg, m) => parseMethod(m, extInfo = Some(ExtensionInformation(isGroupped))))
          ExtensionGroup(parseArgument(extensions(0).arg, _ => "", isExtendedSymbol = true, isGroupped), dMethods)
        }
      }.toList
    }

    def getExtensionGroups: List[ExtensionGroup] = extractExtensionGroups(c.symbol.classMethods)

    def getImplicitConversions: List[ImplicitConversion] =
      val conversionSymbol = Symbol.requiredClass("scala.Conversion")
      val inherited = c.getNonTrivialInheritedMemberTrees
      val inheritedDefDefSymbols = inherited.collect{ case dd: DefDef => dd.symbol }.toList
      val givenFields = (membersToDocument ++ inherited).collect {
          case vd: ValDef if vd.symbol.flags.is(Flags.Given) => vd
        }.toList
        .filter(_.tpt.tpe.derivesFrom(conversionSymbol))
        .map(vd => parseValDef(vd) -> vd.tpt.tpe.baseType(conversionSymbol))

      val implicitVals = (membersToDocument ++ inherited).collect {
          case vd: ValDef if vd.symbol.flags.is(Flags.Implicit) => vd
        }
        .toList
        .filter(_.tpt.tpe.derivesFrom(conversionSymbol))
        .map(vd => parseValDef(vd) -> vd.tpt.tpe.baseType(conversionSymbol))

      val implicitConversionDefs = (getMethods ++ inheritedDefDefSymbols)
        .filter(sym => sym.flags.is(Flags.Implicit) && (sym.paramSymss.size == 0 || (sym.paramSymss.size == 1 && sym.paramSymss(0).size == 0)))
        .toList
        .map(_.tree.asInstanceOf[DefDef])
        .filter(_.returnTpt.tpe.derivesFrom(conversionSymbol))
        .map(m => parseMethod(m.symbol) -> m.returnTpt.tpe.baseType(conversionSymbol))

      val implicitDefs = (getMethods ++ inheritedDefDefSymbols).filter(_.flags.is(Flags.Implicit))
      .filter(m => m.paramSymss.size == 1 && m.paramSymss(0).size == 1)
      .map(m => parseMethod(m) -> m.tree.asInstanceOf[DefDef])
      
      val conversions = (givenFields ++ implicitVals ++ implicitConversionDefs).map {
        case (d: Documentable, AppliedType(tpe, tpeArgs)) => (tpeArgs(0), tpeArgs(1)) match {
          case (t1: Type, t2: Type) => ImplicitConversion(d, t1.typeSymbol.dri, t2.typeSymbol.dri)
        }
      } ++ implicitDefs.map {
        case (d, m) => ImplicitConversion(d, m.paramss(0)(0).tpt.tpe.typeSymbol.dri, m.returnTpt.tpe.typeSymbol.dri)
      }
      conversions

    private def extractGivenMethods(functions: List[Symbol]) = functions
      .filterNot(_.isHiddenByVisibility)
      .filter(_.isGiven)
      .map(m => parseMethod(m, paramPrefix = _ => "using ", isGiven = true))

    def getGivenMethods: List[DFunction] = extractGivenMethods(c.symbol.classMethods)

    private def extractGivenFields(members: List[ValDef]) = members.filter(_.symbol.isGiven).map(parseValDef(_, isGiven = true))

    def getGivenFields: List[DProperty] =
      val valDefs = membersToDocument.collect { case vd: ValDef => vd }.toList
      extractGivenFields(valDefs)

    private def extractMethods(methods: List[Symbol]) = methods.filterNot(s => 
      s.isHiddenByVisibility || 
      s.isGiven || 
      s.isSyntheticFunc || 
      s.isExtensionMethod
    )

    def getMethods: List[Symbol] = extractMethods(c.symbol.classMethods)

    def getInheritedDefinitions: InheritedDefinitions = {
      val trees = c.getNonTrivialInheritedMemberTrees
      
      val inheritedDefDefs = trees
        .collect { case dd: DefDef if !dd.symbol.isClassConstructor => dd.symbol }
        .filterNot(s => s.isSuperBridgeMethod || s.isDefaultHelperMethod)
        .toList
      val inheritedValDefs = trees.collect { case vd: ValDef => vd }.toList
      val inheritedTypeDefs = trees.collect { case td: TypeDef => td }.toList
      val inheritedClassDefs = trees.collect { case cd: ClassDef => cd }.toList
      InheritedDefinitions(
        extractNestedClasslikes(inheritedClassDefs).map(cd => parseClasslike(cd, forSignature = true)),
        extractTypeDefs(inheritedTypeDefs).map(parseTypeDef(_)),
        extractMethods(inheritedDefDefs).map(parseMethod(_)),
        extractValDefs(inheritedValDefs).map(parseValDef(_)),
        extractExtensionGroups(inheritedDefDefs),
        extractGivenFields(inheritedValDefs) ++ extractGivenMethods(inheritedDefDefs)
      )
    }

    def getParents: List[Bound] =
      for
        parentTree <- c.parents if isValidPos(parentTree.pos)  // We assume here that order is correct
        parentSymbol = if parentTree.symbol.isClassConstructor then parentTree.symbol.owner else parentTree.symbol
        if parentSymbol != defn.ObjectClass && parentSymbol != defn.AnyClass
      yield parentTree.dokkaType

    def getSupertypes: List[Bound] = getSupertypes(c).filterNot(s => s == defn.ObjectType || s == defn.AnyType).map(_.dokkaType)

    def getConstructors: List[Symbol] = membersToDocument.collect {
      case d: DefDef if d.symbol.isClassConstructor && c.constructor.symbol != d.symbol => d.symbol
    }.toList

    private def extractNestedClasslikes(classDefs: List[ClassDef]): List[ClassDef] = classDefs.filter(c =>
      c.symbol.shouldDocumentClasslike && 
      !c.symbol.isGiven
    )

    def getNestedClasslikes: List[DClasslike] = extractNestedClasslikes(
      membersToDocument.collect { case c: ClassDef => c }
    )
    .map(c => 
      processTree(c)(parseClasslike(c)) 
    ).toList.flatten

    def getParameterModifier(parameter: Symbol): String =
      val fieldSymbol = c.symbol.field(parameter.name)
      if fieldSymbol.flags.is(Flags.Mutable) then "var "
      else if fieldSymbol.flags.is(Flags.ParamAccessor) && !c.symbol.flags.is(Flags.Case) && !fieldSymbol.flags.is(Flags.Private) then "val "
      else ""

    def getTypeParams: List[TypeDef] = c.body.collect { case targ: TypeDef => targ  }.filter(_.symbol.isTypeParam)

    private def extractTypeDefs(typeDefs: List[TypeDef]) = typeDefs.filter(td =>
      !td.symbol.flags.is(Flags.Synthetic) && 
      (!td.symbol.flags.is(Flags.Case) || !td.symbol.flags.is(Flags.Enum))
    )

    def getTypeDefs: List[TypeDef] = extractTypeDefs(
      membersToDocument.collect { case td: TypeDef => td }.toList
    )

    private def extractValDefs(valDefs: List[ValDef]) = valDefs.filter(vd =>
      !isSyntheticField(vd.symbol, c) && 
      (!vd.symbol.flags.is(Flags.Case) || !vd.symbol.flags.is(Flags.Enum))
    )

    def getValDefs: List[ValDef] = extractValDefs(
      membersToDocument.collect { case vd: ValDef => vd }
    )

    def getCompanion: Option[DRI] = c.symbol.getCompanionSymbol
      .filter(!_.flags.is(Flags.Synthetic))
      .filterNot(_.isHiddenByVisibility)
      .map(_.dri)

    def getConstructorMethod: Option[DFunction] =
      Some(c.constructor.symbol).filter(_.exists).filterNot(_.isHiddenByVisibility).map( d =>
        parseMethod(d, constructorWithoutParamLists(c), s => c.getParameterModifier(s))
      )

  def parseClasslike(classDef: reflect.ClassDef, forSignature: Boolean = false)(using ctx: Context): DClass = classDef match
    case c: ClassDef if classDef.symbol.flags.is(Flags.Object) => parseObject(c, forSignature)
    case c: ClassDef if classDef.symbol.flags.is(Flags.Trait) => parseTrait(c, forSignature)
    case c: ClassDef if classDef.symbol.flags.is(Flags.Enum) => parseEnum(c, forSignature)
    case clazz => parseClass(clazz, forSignature)

  def parseObject(classDef: reflect.ClassDef, forSignature: Boolean = false)(using ctx: Context): DClass =
    val modifier = classDef.symbol.getModifier() match
      case ScalaModifier.Final => ScalaModifier.Empty
      case other => other
    if forSignature then DClass.parseForSignatureOnly(classDef)(
      name = classDef.name.stripSuffix("$"),
      modifier = Map(sourceSet.getSourceSet -> modifier),
      kind = Kind.Object
    ) 
    else DClass(classDef)(
      name = classDef.name.stripSuffix("$"),
      modifier = Map(sourceSet.getSourceSet -> modifier),
      kind = Kind.Object
    )


  def parseEnum(classDef: reflect.ClassDef, forSignature: Boolean = false)(using ctx: Context): DClass =
    val extraModifiers = classDef.symbol.getExtraModifiers().filter(_ != ScalaOnlyModifiers.Sealed)
    val modifier = classDef.symbol.getModifier() match {
      case ScalaModifier.Abstract => ScalaModifier.Empty
      case other => other
    }
    val companion = classDef.symbol.getCompanionSymbol.map(_.tree.asInstanceOf[ClassDef]).get

    val enumVals = companion.membersToDocument.collect {
      case vd: ValDef if !isSyntheticField(vd.symbol, classDef) && vd.symbol.flags.is(Flags.Enum) && vd.symbol.flags.is(Flags.Case) => vd
    }.toList.map(v => parseValDef(v)).map(p => p.withNewExtras(p.getExtra plus IsEnumEntry.Val))

    val enumTypes = companion.membersToDocument.collect {
      case td: TypeDef if !td.symbol.flags.is(Flags.Synthetic) && td.symbol.flags.is(Flags.Enum) && td.symbol.flags.is(Flags.Case) => td
    }.toList.map(parseTypeDef).map(p => p.withNewExtras(p.getExtra plus IsEnumEntry.Type))

    val enumNested = companion.membersToDocument.collect {
      case c: ClassDef if c.symbol.flags.is(Flags.Case) && c.symbol.flags.is(Flags.Enum) => processTree(c)(parseClasslike(c))
    }.flatten.toList.map(p => p.withNewExtras(p.getExtra plus IsEnumEntry.Class))

    if forSignature then DClass.parseForSignatureOnly(classDef)(
      modifier = Map(sourceSet.getSourceSet -> modifier),
      kind = Kind.Enum,
      additionalExtras = Seq()
    )
    else DClass(classDef)(
      modifier = Map(sourceSet.getSourceSet -> modifier),
      kind = Kind.Enum,
      additionalExtras = Seq(EnumExtension(enumVals ++ enumTypes ++ enumNested))
    )

  def parseTrait(classDef: reflect.ClassDef, forSignature: Boolean = false)(using ctx: Context): DClass =
    if forSignature then DClass.parseForSignatureOnly(classDef)(
      kind = Kind.Trait,
    ) 
    else DClass(classDef)(
      kind = Kind.Trait,
    )


  def parseClass(classDef: reflect.ClassDef, forSignature: Boolean = false)(using ctx: Context): DClass =
    if forSignature then DClass.parseForSignatureOnly(classDef)(
      kind = Kind.Class
    )
    else DClass(classDef)(
      kind = Kind.Class
    )

  def parseMethod(
      methodSymbol: Symbol,
      emptyParamsList: Boolean = false,
      paramPrefix: Symbol => String = _ => "",
      extInfo: Option[ExtensionInformation] = None,
      isGiven: Boolean = false
    ): DFunction =
    val annotations = AnnotationsInfo(methodSymbol.getAnnotations())
    val method = methodSymbol.tree.asInstanceOf[DefDef]
    val paramLists = if emptyParamsList then Nil else method.paramss
    val genericTypes = if (methodSymbol.isClassConstructor) Nil else method.typeParams
    val name =
      if methodSymbol.isClassConstructor then "this"
      else if extInfo.isDefined then methodSymbol.name.stripPrefix("extension_")
      else if isGiven then methodSymbol.name.stripPrefix("given_")
      else methodSymbol.name


    def getGivenInstance: Option[Bound] = {
      def extractTypeSymbol(t: Tree): Option[Symbol] = t match
        case tpeTree: TypeTree =>
          inner(tpeTree.tpe)
        case other => None

      def inner(tpe: TypeOrBounds): Option[Symbol] = tpe match
        case ThisType(tpe) => inner(tpe)
        case AnnotatedType(tpe, _) => inner(tpe)
        case AppliedType(tpe, typeOrBoundsList) => inner(tpe)
        case tp @ TermRef(qual, typeName) =>
          qual match
            case _: Type | _: NoPrefix => Some(tp.termSymbol)
            case other => None
        case tp @ TypeRef(qual, typeName) =>
          qual match
            case _: Type | _: NoPrefix => Some(tp.typeSymbol)
            case other => None

      val typeSymbol = extractTypeSymbol(method.returnTpt)

      typeSymbol.map(_.tree).collect {
        case c: ClassDef => c.getParents.headOption
        case _ => Some(method.returnTpt.dokkaType)
      }.flatten
    }
    val optionalExtras = Seq(Option.when(isGiven)(IsGiven(getGivenInstance))).flatten


    new DFunction(
      methodSymbol.dri,
      name,
      /*isConstructor =*/ methodSymbol.isClassConstructor,
      /*parameters =*/ paramLists.flatten.map(parseArgument(_, paramPrefix)).asJava, // TODO add support for parameters
      /*documentation =*/ methodSymbol.documentation.asJava,
      /*expectPresentInSet =*/ null, // unused
      /*sources =*/ methodSymbol.source.asJava,
      /*visibility =*/ sourceSet.asMap(methodSymbol.getVisibility()),
      /*type =*/ method.returnTpt.dokkaType,
      /*generics =*/ genericTypes.map(parseTypeArgument).asJava,
      /*receiver =*/ null, // Not used
      /*modifier =*/ sourceSet.asMap(methodSymbol.getModifier()),
      sourceSet.toSet(),
      PropertyContainer.Companion.empty()
        plus MethodExtension(paramLists.map(_.size), extInfo)
        plus AdditionalModifiers(sourceSet.asMap(methodSymbol.getExtraModifiers().asJava))
        plus(annotations)
        addAll optionalExtras.asJava
    )

  def parseArgument(argument: ValDef, prefix: Symbol => String, isExtendedSymbol: Boolean = false, isGrouped: Boolean = false): DParameter =
    val annotations = AnnotationsInfo(argument.symbol.getAnnotations())
    new DParameter(
      argument.symbol.dri,
      prefix(argument.symbol) + argument.symbol.name,
      argument.symbol.documentation.asJava,
      null,
      argument.tpt.dokkaType,
      sourceSet.toSet(),
      PropertyContainer.Companion.empty()
        .plus(ParameterExtension(isExtendedSymbol, isGrouped))
        .plus(annotations)
    )

  def parseTypeArgument(argument: TypeDef): DTypeParameter =
    // Not sure if we should have such hacks...
    val variancePrefix =
      if  argument.symbol.flags.is(Flags.Covariant) then "+"
      else if argument.symbol.flags.is(Flags.Contravariant) then "-"
      else ""

    new DTypeParameter(
      argument.symbol.dri,
      variancePrefix + argument.symbol.name,
      argument.symbol.documentation.asJava,
      null,
      List(argument.rhs.dokkaType).asJava,
      sourceSet.toSet(),
      PropertyContainer.Companion.empty()
    )

  def parseTypeDef(typeDef: TypeDef): DProperty =
    val annotations = AnnotationsInfo(typeDef.symbol.getAnnotations())

    def isTreeAbstract(typ: Tree): Boolean = typ match {
      case TypeBoundsTree(_, _) => true
      case LambdaTypeTree(params, body) => isTreeAbstract(body)
      case _ => false
    }

    val isAbstract = isTreeAbstract(typeDef.rhs)

    val isTypeOpaque = typeDef.symbol.isOpaque

    val (generics, tpeTree) = typeDef.rhs match
      case LambdaTypeTree(params, body) => (params.map(parseTypeArgument), body)
      case tpe => (Nil, tpe)

    val extraModifiers = Set(Option.when(isTypeOpaque)(ScalaOnlyModifiers.Opaque)).flatten

    new DProperty(
      typeDef.symbol.dri,
      typeDef.name,
      /*documentation =*/ typeDef.symbol.documentation.asJava,
      /*expectPresentInSet =*/ null, // unused
      /*sources =*/ typeDef.symbol.source.asJava,
      /*visibility =*/ sourceSet.asMap(typeDef.symbol.getVisibility()),
      /*type =*/ tpeTree.dokkaType, // TODO this may be hard...
      /*receiver =*/ null, // Not used
      /*setter =*/ null,
      /*getter =*/ null,
      /*modifier =*/ sourceSet.asMap(typeDef.symbol.getModifier()),
      sourceSet.toSet(),
      /*generics =*/ generics.asJava, // TODO
      PropertyContainer.Companion.empty()
        plus PropertyExtension("type", isAbstract)
        plus AdditionalModifiers(sourceSet.asMap(extraModifiers.asJava))
        plus annotations
    )

  def parseValDef(valDef: ValDef, isGiven: Boolean = false): DProperty = {
    val annotations = AnnotationsInfo(valDef.symbol.getAnnotations())
      def givenInstance = Some(valDef.symbol.moduleClass)
          .filter(_.exists)
          .map(_.tree.asInstanceOf[ClassDef])
          .map(_.getParents)
          .filter(!_.isEmpty)
          .map(_.headOption)
          .flatten
      val optionalExtras = Seq(Option.when(isGiven)(IsGiven(givenInstance))).flatten
      new DProperty(
        valDef.symbol.dri,
        valDef.name,
        /*documentation =*/ valDef.symbol.documentation.asJava,
        /*expectPresentInSet =*/ null, // unused
        /*sources =*/ valDef.symbol.source.asJava,
        /*visibility =*/ sourceSet.asMap(valDef.symbol.getVisibility()),
        /*type =*/ valDef.tpt.dokkaType,
        /*receiver =*/ null, // Not used
        /*setter =*/ null,
        /*getter =*/ null,
        /*modifier =*/ sourceSet.asMap(valDef.symbol.getModifier()),
        sourceSet.toSet(),
        /*generics =*/ Nil.asJava,
        PropertyContainer.Companion.empty() plus
          PropertyExtension(
            if valDef.symbol.flags.is(Flags.Mutable) then "var" else "val",
            valDef.symbol.flags.is(Flags.Abstract))
          plus AdditionalModifiers(sourceSet.asMap(valDef.symbol.getExtraModifiers().asJava))
          plus annotations
          addAll optionalExtras.asJava
      )
  }
