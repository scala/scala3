package dotty.dokka


import org.jetbrains.dokka.plugability._
import org.jetbrains.dokka.transformers.sources._

import org.jetbrains.dokka.DokkaConfiguration
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.links._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.base.parsers._
import org.jetbrains.dokka.plugability.DokkaContext
import dokka.java.api._
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties.PropertyContainer
import java.util.{List => JList}

object DottyReader:
  def parseClasslike(fqn: String, sourceSet: SourceSetWrapper, parser: Parser): DClass =
    val segments = fqn.split('.')
    val name = segments.last
    val dri = new DRI(segments.drop(1).mkString("."), name, null, PointingToDeclaration.INSTANCE, null)
    
    val constr: JList[DFunction] = Nil.asJava
    val funs: JList[DFunction] = Nil.asJava
    val props: JList[DProperty] = Nil.asJava
    val nested: JList[DClasslike] = Nil.asJava
    

    val source = new DocumentableSource():
          override def getPath: String = segments.mkString("src/","/", ".scala")
    
    val vis = KotlinVisibility.Public.INSTANCE
    val doc = sourceSet.asMap(parser.parse("## Documentation for Foo"))
    val companion : DObject = null

    val gens: JList[DTypeParameter] = Nil.asJava
    val supers: JList[DriWithKind] = Nil.asJava
    val mod = sourceSet.asMap(JavaModifier.Abstract.INSTANCE)

    new DClass(
        dri,
        "Foo",
        Nil.asJava,
        Nil.asJava,
        Nil.asJava,
        Nil.asJava,
        sourceSet.asMap(source),
        sourceSet.asMap(KotlinVisibility.Public.INSTANCE),
        null,
        Nil.asJava,
        Map().asJava,
        sourceSet.asMap(parser.parse("## Documentation for Foo")),
        null,
        mod,
        sourceSet.toSet,
        PropertyContainer.Companion.empty()
      )      
