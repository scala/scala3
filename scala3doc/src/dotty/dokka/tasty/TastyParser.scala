package dotty.dokka
package tasty

import org.jetbrains.dokka.plugability._
import org.jetbrains.dokka.transformers.sources._

import org.jetbrains.dokka.DokkaConfiguration
import org.jetbrains.dokka.model._
import org.jetbrains.dokka.model.doc._
import org.jetbrains.dokka.base.parsers._
import org.jetbrains.dokka.plugability.DokkaContext
import collection.JavaConverters._
import org.jetbrains.dokka.model.properties.PropertyContainer
import org.jetbrains.dokka.model.properties.PropertyContainerKt._
import org.jetbrains.dokka.model.properties.{WithExtraProperties}

import quoted.Quotes
import scala.tasty.inspector.DocTastyInspector
import dotty.dokka.model.api.withNewMembers
import dotty.dokka.tasty.comments.MemberLookup
import dotty.dokka.tasty.comments.QueryParser
import scala.util.Try

/** Responsible for collectively inspecting all the Tasty files we're interested in.
  *
  * Delegates most of the work to [[TastyParser]] [[dotty.dokka.tasty.TastyParser]].
  */
case class DokkaTastyInspector(parser: Parser)(using ctx: DocContext) extends DocTastyInspector:

  private val topLevels = Seq.newBuilder[Documentable]

  def processCompilationUnit(using q: Quotes)(root: q.reflect.Tree): Unit =
    // NOTE we avoid documenting definitions in the magical stdLibPatches directory;
    // the symbols there are "patched" through dark Dotty magic onto other stdlib
    // definitions, so if we documented their origin, we'd get defs with duplicate DRIs
    if !root.symbol.fullName.startsWith("scala.runtime.stdLibPatches") then
      val parser = new TastyParser(q, this)

      def driFor(link: String): Option[DRI] =
        val symOps = new SymOps[q.type](q)
        import symOps._
        Try(QueryParser(link).readQuery()).toOption.flatMap(q =>
          MemberLookup.lookupOpt(q, None).map{ case (sym, _) => sym.dri}
        )

      ctx.staticSiteContext.foreach(_.memberLinkResolver = driFor)
      topLevels ++= parser.parseRootTree(root.asInstanceOf[parser.qctx.reflect.Tree])
  end processCompilationUnit

  def result(): List[DPackage] =
    topLevels.clear()
    val filePaths = ctx.args.tastyFiles.map(_.getAbsolutePath).toList
    val classpath = ctx.args.classpath.split(java.io.File.pathSeparator).toList

    inspectFilesInContext(classpath, filePaths)

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
            JList(), // TODO add support for other things like type or package object entries
            JList(),
            f.getDocumentation,
            null,
            JSet(ctx.sourceSet),
            f.getExtra
          ).withNewMembers(entries.filterNot(_.isInstanceOf[DPackage]).toList).asInstanceOf[DPackage]
        )
        found.getOrElse(throw IllegalStateException("No package for entries found"))
      }
    }.toList

  extension (self: DPackage) def mergeWith(other: DPackage): DPackage =
    def nodes(p: DPackage): JList[TagWrapper] = p.getDocumentation.get(ctx.sourceSet) match
      case null => JList[TagWrapper]()
      case node => node.getChildren

    mergeExtras(
      DPackage(
        self.getDri,
        (self.getFunctions.asScala ++ other.getFunctions.asScala).asJava,
        (self.getProperties.asScala ++ other.getProperties.asScala).asJava,
        JList(), // WARNING Merging is done before collecting classlikes, if it changes it needs to be refactored
        JList(),
        ctx.sourceSet.toMap(DocumentationNode(nodes(self) ++ nodes(other))),
        null,
        ctx.sourceSet.toSet,
        PropertyContainer.Companion.empty()
      ),
      self,
      other
    )

/** Parses a single Tasty compilation unit. */
case class TastyParser(qctx: Quotes, inspector: DokkaTastyInspector)(using val ctx: DocContext)
    extends ScaladocSupport with BasicSupport with TypesSupport with ClassLikeSupport with SyntheticsSupport with PackageSupport with NameNormalizer:
  import qctx.reflect._

  def processTree[T](tree: Tree)(op: => T): Option[T] = try Option(op) catch
    case e: Exception  =>
      report.warning(throwableToString(e), tree.pos)
      None
  def processTreeOpt[T](tree: Tree)(op: => Option[T]): Option[T] = try op catch
    case e: Exception =>
      report.warning(throwableToString(e), tree.pos)
      None

  def processSymbol[T](sym: Symbol)(op: => T): Option[T] = try Option(op) catch
    case t: Throwable =>
      try report.warning(throwableToString(t), sym.tree.pos) catch
        case _: Throwable =>
          report.warning(s"Failed to process ${sym.fullName}:\n${throwableToString(t)}")
      None

  def parseRootTree(root: Tree): Seq[Documentable] =
    val docs = Seq.newBuilder[Documentable]
    object Traverser extends TreeTraverser:
      var seen: List[Tree] = Nil

      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        seen = tree :: seen
        tree match {
          case pck: PackageClause =>
            docs += parsePackage(pck)
            super.traverseTree(tree)(owner)
          case packageObject: ClassDef if(packageObject.symbol.name.contains("package$")) =>
            docs += parsePackageObject(packageObject)
          case clazz: ClassDef if clazz.symbol.shouldDocumentClasslike =>
            docs += parseClasslike(clazz)
          case _ =>
        }
        seen = seen.tail

    try Traverser.traverseTree(root)(Symbol.spliceOwner)
    catch case e: Throwable =>
      println(s"Problem parsing ${root.pos}, documentation may not be generated.")
      e.printStackTrace()

    docs.result()

