package dotty.dokka
package tasty


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

case class TastyParser(reflect: Reflection, inspector: DokkaTastyInspector) 
  extends ScaladocSupport with BasicSupport with TypesSupport with ClassLikeSupport:
    import reflect._

    def sourceSet = inspector.sourceSet

    def parseRootTree(root: Tree): Seq[Documentable] = 
      val docs = Seq.newBuilder[Documentable]
      object Traverser extends TreeTraverser:
        override def traverseTree(tree: Tree)(using ctx: Context): Unit = 
          tree match {
            case clazz: ClassDef  =>
              docs += parseClass(clazz)
              //classes += DDClass(clazz.name, "dotty.dokka", clazz.symbol.comment.fold("")(_.raw))  
            case _ =>
          }
          super.traverseTree(tree)

      Traverser.traverseTree(root)(using reflect.rootContext)
      docs.result()

  case class DokkaTastyInspector(sourceSet: SourceSetWrapper, parser: Parser, config: DottyDokkaConfig) extends TastyInspector:

    private val topLevels = Seq.newBuilder[Documentable]
    
    protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit = 
      val parser = new TastyParser(reflect, this)
      topLevels ++= parser.parseRootTree(root.asInstanceOf[parser.reflect.Tree])

    def result(): List[DPackage] = 
      val all = topLevels.result()
      val byPackage = all.filter(_.getDri != null).groupBy(_.getDri().getPackageName()) 
      byPackage.map { case (pck, entries) =>
        new DPackage(
          new DRI(pck, null, null, PointingToDeclaration.INSTANCE, null),
          Nil.asJava,
          Nil.asJava,
          entries.collect{ case d: DClasslike => d }.toList.asJava, // TODO add support for other things like type or package object entries
          Nil.asJava,
          Map.empty.asJava, // TODO find docs for package and search for package object to extract doc
          null,
          sourceSet.toSet,
          PropertyContainer.Companion.empty()
        )
      }.toList
