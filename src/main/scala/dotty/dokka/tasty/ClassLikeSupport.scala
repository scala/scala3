package dotty.dokka.tasty

import org.jetbrains.dokka.model._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc._
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties._
import dotty.dokka._
import org.jetbrains.dokka.base.transformers.documentables.CallableExtensions

trait ClassLikeSupport: 
  self: TastyParser =>
  import reflect._

  extension (c: ClassDef):
      def getExtensionGroups: List[ExtensionGroup] = {
          val extensions = c.symbol.classMethods.filterNot(isSyntheticFunc).filter(isExtensionMethod(_))

          extensions.map(e => (getExtendedSymbol(e).get, e) ).groupBy{
            case (arg, e) => arg.symbol.pos
          }.map{
            case (pos, list) => if(list.size == 1) {
              ExtensionGroup(parseArgument(list(0)(0), _ => "", true, false), List(parseMethod(list(0)(1), extInfo = Some(ExtensionInformation(false)))))
            } else {
              ExtensionGroup(parseArgument(list(0)(0), _ => "", true, true), list.map(f => parseMethod(f(1), extInfo = Some(ExtensionInformation(true)))))
            }
          }.toList
      }

      def getMethods: List[Symbol] = c.symbol.classMethods.filterNot(isSyntheticFunc).filterNot(isExtensionMethod(_))

      def getParents: List[Bound] = {
          for
              parentTree <- c.parents if isValidPos(parentTree.pos)  // We assume here that order is correct
              parentSymbol = if (parentTree.symbol.isClassConstructor) parentTree.symbol.owner else parentTree.symbol 
                  if parentSymbol != defn.ObjectClass 
          yield parentTree.dokkaType
      }

      def getConstructors: List[Symbol] = c.body.collect {
          case d: DefDef if d.name == "<init>" && c.constructor.symbol != d.symbol => d.symbol
      }.toList

      def getNestedClasslikes: List[DClasslike] = c.body.collect {
          case c: ClassDef if c.symbol.shouldDocumentClasslike => processTree(c)(parseClasslike(c))
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



  def parseClasslike(classDef: reflect.ClassDef)(using ctx: Context): DClass = classDef match {
    case c: ClassDef if classDef.symbol.flags.is(Flags.Object) => parseObject(c)
    case c: ClassDef if classDef.symbol.flags.is(Flags.Trait) => parseTrait(c)
    case c: ClassDef if classDef.symbol.flags.is(Flags.Enum) => parseEnum(c)
    case clazz => parseClass(clazz)
  }

  def parseObject(classDef: reflect.ClassDef)(using ctx: Context): DClass = 
    val isEnumCompanion = classDef.symbol.getCompanionSymbol.map(s => s.flags.is(Flags.Enum)).getOrElse(false)
    val modifier = classDef.symbol.getModifier() match
      case ScalaModifier.Final => ScalaModifier.Empty
      case other => other

    new DClass(
        classDef.symbol.dri,
        classDef.name.stripSuffix("$"),
        /*constuctors =*/ classDef.getConstructors.map(parseMethod(_)).asJava,
        /*methods =*/ classDef.getMethods.map(parseMethod(_)).asJava,
        /*fields =*/ (classDef.getTypeDefs.map(parseTypeDef) ++ classDef.getValDefs.map(parseValDef)).asJava,
        /*nested =*/ classDef.getNestedClasslikes.asJava,
        /*sources =*/ classDef.symbol.source,
        /*visibility =*/ sourceSet.asMap(classDef.symbol.getVisibility()),
        /*companion =*/ null,
        /*generics =*/ classDef.getTypeParams.map(parseTypeArgument).asJava,
        /*supertypes =*/ Map.empty.asJava, // Not used
        /*documentation =*/ classDef.symbol.documentation,
        /*expectPresentInSet =*/ null, // unused
        /*modifier =*/ sourceSet.asMap(modifier),
        inspector.sourceSet.toSet,
        PropertyContainer.Companion.empty()
          .plus(ClasslikeExtension(classDef.getParents, None, Kind.Object, classDef.getCompanion, classDef.getExtensionGroups))
          .plus(AdditionalModifiers(sourceSet.asMap(classDef.symbol.getExtraModifiers().asJava)))
      )


  def parseEnum(classDef: reflect.ClassDef)(using ctx: Context): DClass = 
    val constructorMethod = Some(parseMethod(classDef.constructor.symbol, constructorWithoutParamLists(classDef), s => classDef.getParameterModifier(s)))
    val extraModifiers = classDef.symbol.getExtraModifiers().filter(_ != ScalaOnlyModifiers.Sealed)
    val modifier = classDef.symbol.getModifier() match {
      case ScalaModifier.Abstract => ScalaModifier.Empty
      case other => other 
    }
    val companion = classDef.symbol.getCompanionSymbol.map(_.tree.asInstanceOf[ClassDef]).get

    val enumVals = companion.body.collect {
      case td: ValDef if !isSyntheticField(td.symbol, classDef) && td.symbol.flags.is(Flags.Enum) && td.symbol.flags.is(Flags.Case) => td
    }.toList.map(v => parseValDef(v)).map(p => p.withNewExtras(p.getExtra plus IsEnumEntry()))

    val enumTypes = companion.body.collect {
      case td: TypeDef if !td.symbol.flags.is(Flags.Synthetic) && !td.symbol.flags.is(Flags.Private) && td.symbol.flags.is(Flags.Enum) && td.symbol.flags.is(Flags.Case) => td
    }.toList.map(parseTypeDef).map(p => p.withNewExtras(p.getExtra plus IsEnumEntry()))

    val enumNested = companion.body.collect {
      case c: ClassDef if c.symbol.flags.is(Flags.Case) && c.symbol.flags.is(Flags.Enum) => processTree(c)(parseClasslike(c))
    }.flatten.toList.map(p => p.withNewExtras(p.getExtra plus IsEnumEntry()))

    new DClass(
        classDef.symbol.dri,
        classDef.name,
        /*constuctors =*/ classDef.getConstructors.map(parseMethod(_)).asJava,
        /*methods =*/ classDef.getMethods.map(parseMethod(_)).asJava,
        /*fields =*/ (classDef.getTypeDefs.map(parseTypeDef) ++ classDef.getValDefs.map(parseValDef)).asJava,
        /*nested =*/ (classDef.getNestedClasslikes).asJava,
        /*sources =*/ classDef.symbol.source,
        /*visibility =*/ sourceSet.asMap(classDef.symbol.getVisibility()),
        /*companion =*/ null,
        /*generics =*/ classDef.getTypeParams.map(parseTypeArgument).asJava,
        /*supertypes =*/ Map.empty.asJava, // Not used
        /*documentation =*/ classDef.symbol.documentation,
        /*expectPresentInSet =*/ null, // unused
        /*modifier =*/ sourceSet.asMap(modifier),
        inspector.sourceSet.toSet,
        PropertyContainer.Companion.empty()
          .plus(ClasslikeExtension(classDef.getParents, constructorMethod, Kind.Enum, classDef.getCompanion, classDef.getExtensionGroups))
          .plus(AdditionalModifiers(sourceSet.asMap(extraModifiers.asJava)))
          .plus(EnumExtension(enumVals ++ enumTypes ++ enumNested))
      )

  def parseTrait(classDef: reflect.ClassDef)(using ctx: Context): DClass = 
    val constructorMethod = Some(parseMethod(classDef.constructor.symbol, constructorWithoutParamLists(classDef), s => classDef.getParameterModifier(s)))
    new DClass(
        classDef.symbol.dri,
        classDef.name,
        /*constuctors =*/ classDef.getConstructors.map(parseMethod(_)).asJava,
        /*methods =*/ classDef.getMethods.map(parseMethod(_)).asJava,
        /*fields =*/ (classDef.getTypeDefs.map(parseTypeDef) ++ classDef.getValDefs.map(parseValDef)).asJava,
        /*nested =*/ classDef.getNestedClasslikes.asJava,
        /*sources =*/ classDef.symbol.source,
        /*visibility =*/ sourceSet.asMap(classDef.symbol.getVisibility()),
        /*companion =*/ null,
        /*generics =*/ classDef.getTypeParams.map(parseTypeArgument).asJava,
        /*supertypes =*/ Map.empty.asJava, // Not used
        /*documentation =*/ classDef.symbol.documentation,
        /*expectPresentInSet =*/ null, // unused
        /*modifier =*/ sourceSet.asMap(classDef.symbol.getModifier()),
        inspector.sourceSet.toSet,
        PropertyContainer.Companion.empty()
          .plus(ClasslikeExtension(classDef.getParents, constructorMethod, Kind.Trait, classDef.getCompanion, classDef.getExtensionGroups))
          .plus(AdditionalModifiers(sourceSet.asMap(classDef.symbol.getExtraModifiers().asJava)))
      )


  def parseClass(classDef: reflect.ClassDef)(using ctx: Context): DClass =
    val constructorMethod = Some(parseMethod(classDef.constructor.symbol, constructorWithoutParamLists(classDef), s => classDef.getParameterModifier(s)))
    new DClass(
        classDef.symbol.dri,
        classDef.name,
        /*constuctors =*/ classDef.getConstructors.map(parseMethod(_)).asJava,
        /*methods =*/ classDef.getMethods.map(parseMethod(_)).asJava,
        /*fields =*/ (classDef.getTypeDefs.map(parseTypeDef) ++ classDef.getValDefs.map(parseValDef)).asJava,
        /*nested =*/ classDef.getNestedClasslikes.asJava,
        /*sources =*/ classDef.symbol.source,
        /*visibility =*/ sourceSet.asMap(classDef.symbol.getVisibility()),
        /*companion =*/ null,
        /*generics =*/ classDef.getTypeParams.map(parseTypeArgument).asJava,
        /*supertypes =*/ Map.empty.asJava, // Not used
        /*documentation =*/ classDef.symbol.documentation,
        /*expectPresentInSet =*/ null, // unused
        /*modifier =*/ sourceSet.asMap(classDef.symbol.getModifier()),
        inspector.sourceSet.toSet,
        PropertyContainer.Companion.empty()
          .plus(ClasslikeExtension(classDef.getParents, constructorMethod, Kind.Class, classDef.getCompanion, classDef.getExtensionGroups))
          .plus(AdditionalModifiers(sourceSet.asMap(classDef.symbol.getExtraModifiers().asJava)))
      )

  def parseMethod(methodSymbol: Symbol, emptyParamsList: Boolean = false, paramPrefix: Symbol => String = _ => "", extInfo: Option[ExtensionInformation] = None): DFunction =
    val method = methodSymbol.tree.asInstanceOf[DefDef]
    val paramLists = if emptyParamsList then Nil else method.paramss
    val genericTypes = if (methodSymbol.isClassConstructor) Nil else method.typeParams
    val name =  if methodSymbol.isClassConstructor then "this" else if extInfo.isDefined then methodSymbol.name.stripPrefix("extension_") else methodSymbol.name
   
    new DFunction(
      methodSymbol.dri,
      name,
      /*isConstructor =*/ methodSymbol.isClassConstructor,
      /*parameters =*/ paramLists.flatten.map(parseArgument(_, paramPrefix)).asJava, // TODO add support for parameters
      /*documentation =*/ methodSymbol.documentation,
      /*expectPresentInSet =*/ null, // unused
      /*sources =*/ methodSymbol.source,
      /*visibility =*/ sourceSet.asMap(methodSymbol.getVisibility()),
      /*type =*/ method.returnTpt.dokkaType,
      /*generics =*/ genericTypes.map(parseTypeArgument).asJava, 
      /*receiver =*/ null, // Not used
      /*modifier =*/ sourceSet.asMap(methodSymbol.getModifier()),
      sourceSet.toSet(),
      PropertyContainer.Companion.empty() 
        plus MethodExtension(paramLists.map(_.size), extInfo)
        plus AdditionalModifiers(sourceSet.asMap(methodSymbol.getExtraModifiers().asJava))
    )

  def parseArgument(argument: ValDef, prefix: Symbol => String, isExtendedSymbol: Boolean = false, isGrouped: Boolean = false): DParameter = 
    new DParameter(
      argument.symbol.dri,
      prefix(argument.symbol) + argument.symbol.name,
      argument.symbol.documentation,
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
      argument.symbol.documentation,
      null,
      List(argument.rhs.dokkaType).asJava,
      sourceSet.toSet(),
      PropertyContainer.Companion.empty()
    )  
    
  def parseTypeDef(typeDef: TypeDef): DProperty =
    val (generics, tpeTree) =  typeDef.rhs match {
      case LambdaTypeTree(params, body) =>
        (params.map(parseTypeArgument), body)
      case tpe =>
        (Nil, tpe)  
    }

    val isAbstract = tpeTree match {
      case TypeBoundsTree(_, _) => true
      case _ => false
    }

    new DProperty(
      typeDef.symbol.dri,
      typeDef.name,
      /*documentation =*/ typeDef.symbol.documentation,
      /*expectPresentInSet =*/ null, // unused
      /*sources =*/ typeDef.symbol.source,
      /*visibility =*/ sourceSet.asMap(typeDef.symbol.getVisibility()),
      /*type =*/ tpeTree.dokkaType, // TODO this may be hard...
      /*receiver =*/ null, // Not used
      /*setter =*/ null,
      /*getter =*/ null,
      /*modifier =*/ sourceSet.asMap(typeDef.symbol.getModifier()),
      sourceSet.toSet(),
      /*generics =*/ generics.asJava, // TODO 
      PropertyContainer.Companion.empty() plus PropertyExtension("type", isAbstract)
    )
  
  def parseValDef(valDef: ValDef): DProperty =
      new DProperty(
        valDef.symbol.dri,
        valDef.name,
        /*documentation =*/ valDef.symbol.documentation,
        /*expectPresentInSet =*/ null, // unused
        /*sources =*/ valDef.symbol.source,
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
      )