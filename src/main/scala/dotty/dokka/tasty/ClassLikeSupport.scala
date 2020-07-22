package dotty.dokka.tasty

import org.jetbrains.dokka.model._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc._
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties.PropertyContainer

trait ClassLikeSupport: 
  self: TastyParser =>
  import reflect._

  def parseClass(classDef: reflect.ClassDef)(using ctx: Context): DClasslike =

    val parents = for
      parentTree <- classDef.parents
      parentSymbol = if (parentTree.symbol.isClassConstructor) parentTree.symbol.owner else parentTree.symbol 
        if !parentSymbol.flags.is(Flags.Synthetic) // TODO add beter filtering on parent symbols
    yield new DriWithKind(asDRI(parentSymbol), if(parentSymbol.isClassDef) KotlinClassKindTypes.CLASS else KotlinClassKindTypes.INTERFACE)  

    val methods = classDef.symbol.classMethods.filterNot(_.flags.is(Flags.Synthetic))
    val constuctors = classDef.symbol.children.filter(_.isClassConstructor)

    new DClass(
        asDRI(classDef.symbol),
        classDef.name,
        constuctors.map(parseMethod).asJava,
        methods.map(parseMethod).asJava,
        Nil.asJava,
        Nil.asJava,
        sourceSet.asMap(getSource(classDef.symbol)),
        sourceSet.asMap(KotlinVisibility.Public.INSTANCE),
        null,
        Nil.asJava,
        sourceSet.asMap(parents.asJava),
        classDef.symbol.documentation,
        null,
        sourceSet.asMap(JavaModifier.Abstract.INSTANCE),
        inspector.sourceSet.toSet,
        PropertyContainer.Companion.empty()
      )

  def parseMethod(methodSymbol: Symbol): DFunction =
   val method = methodSymbol.tree.asInstanceOf[DefDef]
   val paramLists = method.paramss

    new DFunction(
      methodSymbol.dri,
      methodSymbol.name,
      methodSymbol.isClassConstructor,
      paramLists.flatten.map(parseArgument).asJava, // TODO add support for parameters
      methodSymbol.documentation,
      null,
      methodSymbol.source,
      sourceSet.asMap(KotlinVisibility.Public.INSTANCE),
      method.returnTpt.tpe.typeSymbol.dokkaType, // TODO play with it?
      /*generics =*/ method.typeParams.map(parseTypeArgument).asJava, 
      /*receiver =*/ null,
      /*modifier =*/ sourceSet.asMap(JavaModifier.Abstract.INSTANCE),
      sourceSet.toSet(),
      PropertyContainer.Companion.empty()
    )

  def parseArgument(argument: ValDef): DParameter = 
    new DParameter(
      argument.symbol.dri,
      argument.symbol.name,
      argument.symbol.documentation,
      null,
      argument.tpt.tpe.typeSymbol.dokkaType, // Can we get type symbol easier?
      sourceSet.toSet(),
      PropertyContainer.Companion.empty()
    )
    
  def parseTypeArgument(argument: TypeDef): DTypeParameter = 
    new DTypeParameter(
      argument.symbol.declarationDri, // TODO do we need that? Maybe we can suppot TypeDef in .dri method?
      argument.symbol.name,
      argument.symbol.documentation,
      null,
      Nil.asJava, // TODO add type bounds
      sourceSet.toSet(),
      PropertyContainer.Companion.empty()
    )  