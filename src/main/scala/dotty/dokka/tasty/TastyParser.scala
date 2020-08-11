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
import org.jetbrains.dokka.model.properties.{WithExtraProperties}
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

    def processTree[T](tree: Tree)(op: => T): Option[T] = try Option(op) catch case e: Throwable => errorMsg(tree, tree.symbol.show, e)
    def processSymbol[T](sym: Symbol)(op: => T): Option[T] = try Option(op) catch case e: Throwable => errorMsg(sym, sym.show, e)

    private def errorMsg[T](a: Any, m: => String, e: Throwable): Option[T] = 
      val msg = try m catch case e: Throwable => a.toString
      println(s"ERROR: tree is faling: msg")
      e.printStackTrace()
      throw e    

    def parseRootTree(root: Tree): Seq[Documentable] =
      val docs = Seq.newBuilder[Documentable]
      object Traverser extends TreeTraverser:
        var seen: List[Tree] = Nil 

        override def traverseTree(tree: Tree)(using ctx: Context): Unit = 
          seen = tree :: seen
          tree match {
            case pck: PackageClause => 
              docs += parsePackage(pck)
              super.traverseTree(tree)
            case packageObject: ClassDef if(packageObject.symbol.name.contains("package$")) =>
              docs += parsePackageObject(packageObject)
            case clazz: ClassDef if clazz.symbol.shouldDocumentClasslike =>
              docs += parseClasslike(clazz)
            case _ =>  
          }
          seen = seen.tail

      try Traverser.traverseTree(root)(using reflect.rootContext)
      catch case e: Throwable =>
        println(s"Problem parsing ${root.pos}, documentation may not be generated.")
        e.printStackTrace()

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
              f.getFunctions,
              f.getProperties,
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
          (self.getFunctions.asScala ++ other.getFunctions.asScala).asJava,
          (self.getProperties.asScala ++ other.getProperties.asScala).asJava,
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


