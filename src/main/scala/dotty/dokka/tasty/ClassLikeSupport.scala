package dotty.dokka.tasty

import org.jetbrains.dokka.model._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc._
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties._
import dotty.dokka._

trait ClassLikeSupport: 
  self: TastyParser =>
  import reflect._

  def parseClass(classDef: reflect.ClassDef)(using ctx: Context): DClasslike =
    val parents = for
      parentTree <- classDef.parents if isValidPos(parentTree.pos)  // We assume here that order is correct
      parentSymbol = if (parentTree.symbol.isClassConstructor) parentTree.symbol.owner else parentTree.symbol 
        if parentSymbol != defn.ObjectClass
    yield parentTree.dokkaType

    val methods = classDef.symbol.classMethods.filterNot(_.flags.is(Flags.Synthetic))
    val constuctors = classDef.body.collect {
      case d: DefDef if d.name == "<init>" && classDef.constructor.symbol != d.symbol => 
        parseMethod(d.symbol)
    }

    val flags = classDef.symbol.flags
    val kind = 
      if flags.is(Flags.Object) then Kind.Object 
      else if flags.is(Flags.Trait) then Kind.Trait
      else Kind.Class

    val name = if kind == Kind.Object then classDef.name.stripSuffix("$") else classDef.name

    def paramMod(sym: Symbol): String = 
      val fieldSymbol = classDef.symbol.field(sym.name)
      if fieldSymbol.flags.is(Flags.Mutable) then "var "
      else if fieldSymbol.flags.is(Flags.ParamAccessor) && !classDef.symbol.flags.is(Flags.Case) && !fieldSymbol.flags.is(Flags.Private) then "val "
      else ""      

    val constructorMethod = 
      if kind == Kind.Object then None 
      else Some(parseMethod(classDef.constructor.symbol, constructorWithoutParamLists(classDef), paramMod))

    val typeParams = classDef.body.collect { case targ: TypeDef => targ  }.filter(_.symbol.isTypeParam)

    val modifier = classDef.symbol.getModifier() match
      case ScalaModifier.Final if kind == Kind.Object => ScalaModifier.Empty
      case other => other

    new DClass(
        classDef.symbol.dri,
        name,
        /*constuctors =*/ constuctors.asJava,
        /*methods =*/ methods.map(parseMethod(_)).asJava,
        /*fields =*/ Nil.asJava,
        /*nested =*/ Nil.asJava,
        /*sources =*/ classDef.symbol.source,
        /*visibility =*/ sourceSet.asMap(classDef.symbol.getVisibility()),
        /*companion =*/ null,
        /*generics =*/ typeParams.map(parseTypeArgument).asJava,
        /*supertypes =*/ Map.empty.asJava, // Not used
        /*documentation =*/ classDef.symbol.documentation,
        /*expectPresentInSet =*/ null, // unused
        /*modifier =*/ sourceSet.asMap(modifier),
        inspector.sourceSet.toSet,
        PropertyContainer.Companion.empty()
          .plus(ClasslikeExtension(parents, constructorMethod, kind))
          .plus(AdditionalModifiers(sourceSet.asMap(classDef.symbol.getExtraModifiers().asJava)))
      )

  def parseMethod(methodSymbol: Symbol, emptyParamsList: Boolean = false, paramPrefix: Symbol => String = _ => ""): DFunction =
    val method = methodSymbol.tree.asInstanceOf[DefDef]
    val paramLists = if emptyParamsList then Nil else method.paramss
    val genericTypes = if (methodSymbol.isClassConstructor) Nil else method.typeParams
   
    new DFunction(
      methodSymbol.dri,
      if methodSymbol.isClassConstructor then "this" else methodSymbol.name,
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
        plus MethodExtension(paramLists.map(_.size))
        plus AdditionalModifiers(sourceSet.asMap(methodSymbol.getExtraModifiers().asJava))
    )

  def parseArgument(argument: ValDef, prefix: Symbol => String): DParameter = 
    new DParameter(
      argument.symbol.dri,
      prefix(argument.symbol) + argument.symbol.name,
      argument.symbol.documentation,
      null,
      argument.tpt.dokkaType,
      sourceSet.toSet(),
      PropertyContainer.Companion.empty()
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