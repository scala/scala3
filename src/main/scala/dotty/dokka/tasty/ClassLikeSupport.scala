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
      parentTree <- classDef.parents // We assume here that order is correct
      parentSymbol = if (parentTree.symbol.isClassConstructor) parentTree.symbol.owner else parentTree.symbol 
        if parentSymbol != defn.ObjectClass
    yield parentTree.dokkaType

    val methods = classDef.symbol.classMethods.filterNot(_.flags.is(Flags.Synthetic))
    val constuctors = classDef.body.collect {
      case d: DefDef if d.name == "<init>" && classDef.constructor.symbol != d.symbol => 
        parseMethod(d.symbol)
    }

    new DClass(
        classDef.symbol.dri,
        classDef.name,
        /*constuctors =*/ constuctors.asJava,
        /*methods =*/ methods.map(parseMethod).asJava,
        /*fields =*/ Nil.asJava,
        /*nested =*/ Nil.asJava,
        /*sources =*/ classDef.symbol.source,
        /*visibility =*/ sourceSet.asMap(KotlinVisibility.Public.INSTANCE), // TODO add support for visibility
        /*companion =*/ null,
        /*generics =*/ classDef.constructor.typeParams.map(parseTypeArgument).asJava,
        /*supertypes =*/ Map.empty.asJava, // Not used
        /*documentation =*/ classDef.symbol.documentation,
        /*expectPresentInSet =*/ null, // unused
        /*modifier =*/ Map.empty.asJava, // TODO add support for modifers
        inspector.sourceSet.toSet,
        PropertyContainer.Companion.empty().plus(ClasslikeExtension(parents, Some(parseMethod(classDef.constructor.symbol))))
      )

  def parseMethod(methodSymbol: Symbol): DFunction =
    val method = methodSymbol.tree.asInstanceOf[DefDef]
    val paramLists = method.paramss
   
    new DFunction(
      methodSymbol.dri,
      if methodSymbol.isClassConstructor then "this" else methodSymbol.name,
      /*isConstructor =*/ methodSymbol.isClassConstructor,
      /*parameters =*/ paramLists.flatten.map(parseArgument).asJava, // TODO add support for parameters
      /*documentation =*/ methodSymbol.documentation,
      /*expectPresentInSet =*/ null, // unused
      /*sources =*/ methodSymbol.source,
      /*visibility =*/ sourceSet.asMap(KotlinVisibility.Public.INSTANCE), // TODO add support for visibility
      /*type =*/ method.returnTpt.dokkaType,
      /*generics =*/ method.typeParams.map(parseTypeArgument).asJava, 
      /*receiver =*/ null, // Not used
      /*modifier =*/ Map.empty.asJava, // TODO add support for modifers
      sourceSet.toSet(),
      PropertyContainer.Companion.empty() plus MethodExtension(paramLists.map(_.size))
    )

  def parseArgument(argument: ValDef): DParameter = 
    new DParameter(
      argument.symbol.dri,
      argument.symbol.name,
      argument.symbol.documentation,
      null,
      argument.tpt.dokkaType,
      sourceSet.toSet(),
      PropertyContainer.Companion.empty()
    )
    
  def parseTypeArgument(argument: TypeDef): DTypeParameter = 
    new DTypeParameter(
      argument.symbol.dri,
      argument.symbol.name,
      argument.symbol.documentation,
      null,
      List(argument.rhs.dokkaType).asJava,
      sourceSet.toSet(),
      PropertyContainer.Companion.empty()
    )  