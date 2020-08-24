package dotty.dokka.tasty

import org.jetbrains.dokka.model._
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
      supertypes: Map[DokkaConfiguration$DokkaSourceSet, List[DriWithKind]] = Map.empty,
      documentation: Map[DokkaConfiguration$DokkaSourceSet, DocumentationNode] = classDef.symbol.documentation,
      modifier: Map[DokkaConfiguration$DokkaSourceSet, Modifier] = Map(sourceSet.getSourceSet -> classDef.symbol.getModifier()),
      additionalExtras: Seq[ExtraProperty[DClass]] = Seq.empty
    ): DClass = new DClass(
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
              classDef.getInheritedMethods.map(parseMethod(_)),
              classDef.getGivenMethods ++ classDef.getGivenFields
            ))
            .plus(AdditionalModifiers(sourceSet.asMap(classDef.symbol.getExtraModifiers().asJava)))
            .plus(InheritanceInfo(classDef.getSupertypes, List.empty))
            .addAll(additionalExtras.asJava) 
      )

  extension (c: ClassDef):
      def getExtensionGroups: List[ExtensionGroup] = {
          case class ExtensionRepr(arg: ValDef, method: Symbol)
          val extensions = c.symbol.classMethods.filterNot(isSyntheticFunc).filter(isExtensionMethod(_))
            .map(m => ExtensionRepr(getExtendedSymbol(m).get, m))
          val groupped = extensions.groupBy( e => e.arg.pos)
          groupped.map {
            case (pos, extensions) => {
              val isGroupped = extensions.size > 1
              val dMethods = extensions.map( (arg, m) => parseMethod(m, extInfo = Some(ExtensionInformation(isGroupped))))
              ExtensionGroup(parseArgument(extensions(0).arg, _ => "", isExtendedSymbol = true, isGroupped), dMethods)
            }
          }.toList
      }

      def getGivenMethods: List[DFunction] = {
        c.symbol.classMethods.filter(_.isGiven()).map(m => parseMethod(m, paramPrefix = _ => "using ", isGiven = true))
      }

      def getGivenFields: List[DProperty] = {
        val valDefs = c.body.collect { 
          case td: ValDef if td.symbol.isGiven() => td
        }
        
        valDefs.map(parseValDef(_, isGiven = true))
      }

      def getMethods: List[Symbol] = c.symbol.classMethods.filterNot(isSyntheticFunc).filterNot(isExtensionMethod(_)).filterNot(_.isGiven())

      def getInheritedMethods: List[Symbol] = c.symbol.methods.filterNot(isSyntheticFunc).filterNot(isExtensionMethod(_))
          .filter(s => s.maybeOwner != c.symbol)

      def getParents: List[Bound] = {
          for
              parentTree <- c.parents if isValidPos(parentTree.pos)  // We assume here that order is correct
              parentSymbol = if (parentTree.symbol.isClassConstructor) parentTree.symbol.owner else parentTree.symbol 
                  if parentSymbol != defn.ObjectClass 
          yield parentTree.dokkaType
      }

      def getSupertypes: List[Bound] = getSupertypes(c).map(_.dokkaType)

      def getConstructors: List[Symbol] = c.body.collect {
          case d: DefDef if d.name == "<init>" && c.constructor.symbol != d.symbol => d.symbol
      }.toList

      def getNestedClasslikes: List[DClasslike] = c.body.collect {
          case c: ClassDef if c.symbol.shouldDocumentClasslike && !c.symbol.isGiven() => processTree(c)(parseClasslike(c))
        }.flatten.toList

      def getParameterModifier(parameter: Symbol): String = {
          val fieldSymbol = c.symbol.field(parameter.name)
          if fieldSymbol.flags.is(Flags.Mutable) then "var "
          else if fieldSymbol.flags.is(Flags.ParamAccessor) && !c.symbol.flags.is(Flags.Case) && !fieldSymbol.flags.is(Flags.Private) then "val "
          else ""  
      }

      def getTypeParams: List[TypeDef] = c.body.collect { case targ: TypeDef => targ  }.filter(_.symbol.isTypeParam)

      def getTypeDefs: List[TypeDef] = c.body.collect { 
        case td: TypeDef if !td.symbol.flags.is(Flags.Synthetic) && !td.symbol.flags.is(Flags.Private) && (!td.symbol.flags.is(Flags.Case) || !td.symbol.flags.is(Flags.Enum))
          => td 
      }

      def getValDefs: List[ValDef] = c.body.collect { 
        case td: ValDef if !isSyntheticField(td.symbol, c) && (!td.symbol.flags.is(Flags.Case) || !td.symbol.flags.is(Flags.Enum))
          => td
      }

      def getCompanion: Option[DRI] = c.symbol.getCompanionSymbol
          .filter(!_.flags.is(Flags.Synthetic))
          .map(_.dri)

      def getConstructorMethod: Option[DFunction] = Some(c.constructor.symbol).filter(_.exists).map( d =>
          parseMethod(d, constructorWithoutParamLists(c), s => c.getParameterModifier(s))
      )



  def parseClasslike(classDef: reflect.ClassDef)(using ctx: Context): DClass = classDef match {
    case c: ClassDef if classDef.symbol.flags.is(Flags.Object) => parseObject(c)
    case c: ClassDef if classDef.symbol.flags.is(Flags.Trait) => parseTrait(c)
    case c: ClassDef if classDef.symbol.flags.is(Flags.Enum) => parseEnum(c)
    case clazz => parseClass(clazz)
  }

  def parseObject(classDef: reflect.ClassDef)(using ctx: Context): DClass = 
    val modifier = classDef.symbol.getModifier() match
      case ScalaModifier.Final => ScalaModifier.Empty
      case other => other

    DClass(classDef)(
        name = classDef.name.stripSuffix("$"),
        modifier = Map(sourceSet.getSourceSet -> modifier),
        kind = Kind.Object
      )


  def parseEnum(classDef: reflect.ClassDef)(using ctx: Context): DClass = 
    val extraModifiers = classDef.symbol.getExtraModifiers().filter(_ != ScalaOnlyModifiers.Sealed)
    val modifier = classDef.symbol.getModifier() match {
      case ScalaModifier.Abstract => ScalaModifier.Empty
      case other => other 
    }
    val companion = classDef.symbol.getCompanionSymbol.map(_.tree.asInstanceOf[ClassDef]).get

    val enumVals = companion.body.collect {
      case td: ValDef if !isSyntheticField(td.symbol, classDef) && td.symbol.flags.is(Flags.Enum) && td.symbol.flags.is(Flags.Case) => td
    }.toList.map(v => parseValDef(v)).map(p => p.withNewExtras(p.getExtra plus IsEnumEntry.Val))

    val enumTypes = companion.body.collect {
      case td: TypeDef if !td.symbol.flags.is(Flags.Synthetic) && !td.symbol.flags.is(Flags.Private) && td.symbol.flags.is(Flags.Enum) && td.symbol.flags.is(Flags.Case) => td
    }.toList.map(parseTypeDef).map(p => p.withNewExtras(p.getExtra plus IsEnumEntry.Type))

    val enumNested = companion.body.collect {
      case c: ClassDef if c.symbol.flags.is(Flags.Case) && c.symbol.flags.is(Flags.Enum) => processTree(c)(parseClasslike(c))
    }.flatten.toList.map(p => p.withNewExtras(p.getExtra plus IsEnumEntry.Class))

    DClass(classDef)(
        modifier = Map(sourceSet.getSourceSet -> modifier),
        kind = Kind.Enum,
        additionalExtras = Seq(EnumExtension(enumVals ++ enumTypes ++ enumNested)) 
      )

  def parseTrait(classDef: reflect.ClassDef)(using ctx: Context): DClass = 
    DClass(classDef)(
        kind = Kind.Trait,
    )


  def parseClass(classDef: reflect.ClassDef)(using ctx: Context): DClass = 
    DClass(classDef)(
        kind = Kind.Class
    )

  def parseMethod(
      methodSymbol: Symbol, 
      emptyParamsList: Boolean = false, 
      paramPrefix: Symbol => String = _ => "", 
      extInfo: Option[ExtensionInformation] = None,
      isGiven: Boolean = false
    ): DFunction =
    val method = methodSymbol.tree.asInstanceOf[DefDef]
    val paramLists = if emptyParamsList then Nil else method.paramss
    val genericTypes = if (methodSymbol.isClassConstructor) Nil else method.typeParams
    val name =  if methodSymbol.isClassConstructor then "this" 
      else if extInfo.isDefined then methodSymbol.name.stripPrefix("extension_") 
      else if isGiven then methodSymbol.name.stripPrefix("given_")
      else methodSymbol.name
    

    def getGivenInstance: Option[Bound] = {
      def extractTypeSymbol(t: Tree): Option[Symbol] = t match {
          case tpeTree: TypeTree =>  
            inner(tpeTree.tpe) 
          case other => None
      }

      def inner(tpe: TypeOrBounds): Option[Symbol] = { 
        tpe match {
          case ThisType(tpe) => inner(tpe)
          case AnnotatedType(tpe, _) => inner(tpe)
          case AppliedType(tpe, typeOrBoundsList) => inner(tpe)
          case tp @ TermRef(qual, typeName) => 
            qual match {
              case _: Type | _: NoPrefix => Some(tp.termSymbol)
              case other => None
              } 
          case tp @ TypeRef(qual, typeName) =>
              qual match {
              case _: Type | _: NoPrefix => Some(tp.typeSymbol)
              case other => None
              } 
      }
    }

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
        addAll optionalExtras.asJava
    )

  def parseArgument(argument: ValDef, prefix: Symbol => String, isExtendedSymbol: Boolean = false, isGrouped: Boolean = false): DParameter = 
    new DParameter(
      argument.symbol.dri,
      prefix(argument.symbol) + argument.symbol.name,
      argument.symbol.documentation.asJava,
      null,
      argument.tpt.dokkaType,
      sourceSet.toSet(),
      PropertyContainer.Companion.empty()
        .plus(ParameterExtension(isExtendedSymbol, isGrouped))
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

    def isTreeAbstract(typ: Tree): Boolean = typ match {
      case TypeBoundsTree(_, _) => true
      case LambdaTypeTree(params, body) => isTreeAbstract(body)
      case _ => false
    }

    val isAbstract = isTreeAbstract(typeDef.rhs)

    val isOpaque = hackIsOpaque(self.reflect)(typeDef.symbol)

    val (generics, tpeTree) =  typeDef.rhs match {
      case LambdaTypeTree(params, body) if isAbstract || isOpaque =>
        (params.map(parseTypeArgument), body)
      case tpe =>
        (Nil, tpe)  
    }

    val extraModifiers = Set(Option.when(isOpaque)(ScalaOnlyModifiers.Opaque)).flatten

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
    )
  
  def parseValDef(valDef: ValDef, isGiven: Boolean = false): DProperty = {
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
          addAll optionalExtras.asJava
      )
  }