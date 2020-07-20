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
    val mod = sourceSet.asMap(JavaModifier.Abstract.INSTANCE)
    val doc = classDef.symbol.comment match 
      case Some(comment) => 
        sourceSet.asMap(parseComment(comment, classDef))
      case None =>  
        Map.empty.asJava
    
 
    val parents = classDef.parents.map { parentTree =>
      val parentSymbol = if(parentTree.symbol.isClassConstructor) parentTree.symbol.owner else parentTree.symbol
      new DriWithKind(asDRI(parentSymbol), if(parentSymbol.isClassDef) KotlinClassKindTypes.CLASS else KotlinClassKindTypes.INTERFACE)
    }

    new DClass(
        asDRI(classDef.symbol),
        classDef.name,
        Nil.asJava,
        Nil.asJava,
        Nil.asJava,
        Nil.asJava,
        sourceSet.asMap(getSource(classDef.symbol)),
        sourceSet.asMap(KotlinVisibility.Public.INSTANCE),
        null,
        Nil.asJava,
        sourceSet.asMap(parents.asJava),
        doc,
        null,
        mod,
        inspector.sourceSet.toSet,
        PropertyContainer.Companion.empty()
      )
