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
  extends ScaladocSupport with BasicSupport with TypesSupport with ClassLikeSupport with SyntheticsSupport with PackageSupport:
    import reflect._

    def sourceSet = inspector.sourceSet

    def parseRootTree(root: Tree): Seq[Documentable] = 
      val docs = Seq.newBuilder[Documentable]
      object Traverser extends TreeTraverser:
        override def traverseTree(tree: Tree)(using ctx: Context): Unit = 
          tree match {
            case pck: PackageClause => 
              docs += parsePackage(pck)
            case packageObject: ClassDef if(packageObject.symbol.name == "package$") =>
              docs += parsePackageObject(packageObject)
            case clazz: ClassDef  =>
              docs += parseClass(clazz)
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
      val packages = all
        .filter(_.isInstanceOf[DPackage])
        .map(_.asInstanceOf[DPackage])
        .groupBy(_.getDri)
        .map((dri, pckgs) =>
          pckgs.reduce(_.mergeWith(_))
        )

      val byPackage = all.filter(_.getDri != null).groupBy(_.getDri().getPackageName())
      byPackage.map { 
        case (pck, entries) => {
          val found = packages.find(d => d.getName == pck)
          .map( f =>
            new DPackage(
              f.getDri,
              Nil.asJava,
              Nil.asJava,
              entries.collect{ case d: DClasslike => d }.toList.asJava, // TODO add support for other things like type or package object entries
              Nil.asJava,
              f.getDocumentation,
              null,
              sourceSet.toSet,
              PropertyContainer.Companion.empty()
            )
          )
          found.getOrElse(throw IllegalStateException("No package for entries found"))
        }
      }.toList

    def (self: DPackage).mergeWith(other: DPackage): DPackage = 
      val doc1 = self.getDocumentation.asScala.get(sourceSet.getSourceSet).map(_.getChildren).getOrElse(Nil.asJava)
      val doc2 = other.getDocumentation.asScala.get(sourceSet.getSourceSet).map(_.getChildren).getOrElse(Nil.asJava)
      DPackage(
          self.getDri,
          Nil.asJava,
          Nil.asJava,
          Nil.asJava, // WARNING Merging is done before collecting classlikes, if it changes it needs to be refactored
          Nil.asJava,
          sourceSet.asMap(
              DocumentationNode(
                  (
                    doc1.asScala ++ doc2.asScala
                  ).asJava
              )
          ),
          null,
          sourceSet.toSet,
          PropertyContainer.Companion.empty()
      )
