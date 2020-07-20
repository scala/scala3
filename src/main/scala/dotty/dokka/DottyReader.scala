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
import dotty.tastydoc.representations._


import scala.tasty.Reflection
import scala.tasty.inspector.TastyInspector
import dotty.tastydoc.representations
import dotty.tastydoc.representations._


case class DottyDokkaParser(reflect: Reflection, sourceSet: SourceSetWrapper, parser: Parser, config: DottyDokkaConfig) extends ScaladocSupport:
  def parseTree(reflect: reflect.Tree): Seq[Documentable] = Nil


class DokkaTastyInspector extends TastyInspector:
  private val packages = Set.newBuilder[DDPackage]
  private val classes = Set.newBuilder[DDClass]

  private val topLevels = Seq.newBuilder[Documentable]
  
  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = 
    import reflect.{_, given _}

    object Traverser extends TreeTraverser:
      override def traverseTree(tree: Tree)(using ctx: Context): Unit = 
        tree match 
          case clazz: ClassDef  =>
            classes += DDClass(clazz.name, "dotty.dokka", clazz.symbol.comment.fold("")(_.raw))  
          case _ =>
        }
        super.traverseTree(tree)

    Traverser.traverseTree(root)(using reflect.rootContext)

  def result() = 
    val res = DDUnit(classes.result.toList, packages.result.toList)
    println(res)
    res

  def mkClass  


case class DottyReader(sourceSet: SourceSetWrapper, parser: Parser, config: DottyDokkaConfig):
  def parsePackage(pck: DDPackage): DPackage =
    val entries = config.compilationUnit.classesByPackage.get(pck.name).getOrElse(Nil).map(parseClasslike(pck.name))

    new DPackage(
        new DRI(pck.name, null, null, PointingToDeclaration.INSTANCE, null),
        Nil.asJava,
        Nil.asJava,
        entries.toList.asJava,
        Nil.asJava,
        sourceSet.asMap(parser.parse(pck.comment)),
        null,
        sourceSet.toSet,
        PropertyContainer.Companion.empty()
    )

  
  def parseClasslike(packatgeName: String)(clazz: DDClass): DClass =
    val dri = new DRI(packatgeName, clazz.name, null, PointingToDeclaration.INSTANCE, null)
    
    val constr: JList[DFunction] = Nil.asJava
    val funs: JList[DFunction] = Nil.asJava
    val props: JList[DProperty] = Nil.asJava
    val nested: JList[DClasslike] = Nil.asJava
    

    val source = new DocumentableSource():
          override def getPath: String = packatgeName.mkString("src/","/", s"${clazz.name}.scala")
    
    val vis = KotlinVisibility.Public.INSTANCE
    val doc = sourceSet.asMap(parser.parse("## Documentation for Foo"))
    val companion : DObject = null

    val gens: JList[DTypeParameter] = Nil.asJava
    val supers: JList[DriWithKind] = Nil.asJava
    val mod = sourceSet.asMap(JavaModifier.Abstract.INSTANCE)

    new DClass(
        dri,
        clazz.name,
        Nil.asJava,
        Nil.asJava,
        Nil.asJava,
        Nil.asJava,
        sourceSet.asMap(source),
        sourceSet.asMap(KotlinVisibility.Public.INSTANCE),
        null,
        Nil.asJava,
        Map().asJava,
        sourceSet.asMap(parser.parse(clazz.comment)),
        null,
        mod,
        sourceSet.toSet,
        PropertyContainer.Companion.empty()
      )      
