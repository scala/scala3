package dotty.tools.scaladoc
package tasty


import java.util.regex.Pattern

import scala.util.{Try, Success, Failure}
import scala.tasty.inspector.DocTastyInspector
import scala.quoted._

import dotty.tools.dotc

import dotty.tools.scaladoc.tasty.comments.MemberLookup
import dotty.tools.scaladoc.tasty.comments.QueryParser
import dotty.tools.scaladoc.tasty.comments.Comment

import java.nio.file.Paths
import java.nio.file.Files

import SymOps._
import ScaladocSupport._

/** Responsible for collectively inspecting all the Tasty files we're interested in.
  *
  * Delegates most of the work to [[TastyParser]] [[dotty.tools.scaladoc.tasty.TastyParser]].
  */
case class ScaladocTastyInspector()(using ctx: DocContext) extends DocTastyInspector:

  private val topLevels = Seq.newBuilder[(String, Member)]
  private var rootDoc: Option[Comment] = None

  def processCompilationUnit(using Quotes)(root: reflect.Tree): Unit = ()

  override def postProcess(using Quotes): Unit =
    // hack into the compiler to get a list of all top-level trees
    // in principle, to do this, one would collect trees in processCompilationUnit
    // however, path-dependent types disallow doing so w/o using casts
    inline def hackForeachTree(thunk: reflect.Tree => Unit): Unit =
      given dctx: dotc.core.Contexts.Context = quotes.asInstanceOf[scala.quoted.runtime.impl.QuotesImpl].ctx
      dctx.run.nn.units.foreach { compilationUnit =>
        // mirrors code from TastyInspector
        thunk(compilationUnit.tpdTree.asInstanceOf[reflect.Tree])
      }

    val symbolsToSkip: Set[reflect.Symbol] =
      ctx.args.identifiersToSkip.flatMap { ref =>
        val qrSymbol = reflect.Symbol
        Try(qrSymbol.requiredPackage(ref)).orElse(Try(qrSymbol.requiredClass(ref))) match {
          case Success(sym) => Some(sym)
          case Failure(err) =>
            report.warning(
              s"Failed to resolve identifier to skip - $ref - because: ${throwableToString(err)}",
              dotc.util.NoSourcePosition,
            )
            None
          }
      }.toSet

    val patternsToSkip: List[Pattern] =
      ctx.args.regexesToSkip.flatMap { regexString =>
        Try(Pattern.compile(regexString)) match
          case Success(pat) => Some(pat)
          case Failure(err) =>
            report.warning(
              s"Failed to compile regex to skip - $regexString - because: ${throwableToString(err)}",
              dotc.util.NoSourcePosition,
            )
            None
      }

    def isSkipped(sym: reflect.Symbol): Boolean =
      def isSkippedById(sym: reflect.Symbol): Boolean =
        if !sym.exists then false else
          symbolsToSkip.contains(sym) || isSkipped(sym.owner)

      def isSkippedByRx(sym: reflect.Symbol): Boolean =
        val symStr = sym.fullName
        patternsToSkip.exists(p => p.matcher(symStr).matches())

      isSkippedById(sym) || isSkippedByRx(sym)

    val parser = new TastyParser(quotes, this)(isSkipped)
    def driFor(link: String): Option[DRI] =
      val symOps = new SymOpsWithLinkCache
      import symOps._
      Try(QueryParser(link).readQuery()).toOption.flatMap(query =>
        MemberLookup.lookupOpt(query, None).map {
          case (sym, _, inheritingParent) => inheritingParent match
            case Some(parent) => sym.driInContextOfInheritingParent(parent)
            case None => sym.dri
        }
      )

    ctx.staticSiteContext.foreach(_.memberLinkResolver = driFor)

    var alreadyProcessed = false
    def processRootDocIfNeeded(tree: parser.qctx.reflect.Tree) =
      def readFile(pathStr: String)(using CompilerContext): Option[String] =
        try
          val path = Paths.get(pathStr)
          if Files.exists(path) then Some(util.IO.read(path))
          else
            report.inform("Rootdoc at $pathStr does not exisits")
            None
        catch
          case e: RuntimeException =>
            report.warning(s"Unable to read root package doc from $pathStr: ${throwableToString(e)}")
            None

      if !alreadyProcessed then
        alreadyProcessed = true
        ctx.args.rootDocPath.flatMap(readFile).map { content =>
          import parser.qctx.reflect._
          def root(s: Symbol): Symbol = if s.owner.isNoSymbol then s else root(s.owner)
          val topLevelPck = root(tree.symbol)
          rootDoc = Some(parseCommentString(using parser.qctx, summon[DocContext])(content, topLevelPck, None))
        }

    hackForeachTree { root =>
      if !isSkipped(root.symbol) then
        val treeRoot = root.asInstanceOf[parser.qctx.reflect.Tree]
        processRootDocIfNeeded(treeRoot)
        topLevels ++= parser.parseRootTree(treeRoot)
    }

    if ctx.args.documentSyntheticTypes then
      import parser.qctx.reflect._
      val intrinsicTypeDefs = parser.intrinsicTypeDefs.toSeq.map { s =>
        "scala" -> parser.parseTypeDef(s.tree.asInstanceOf[TypeDef])
      }
      val intrinsicClassDefs = parser.intrinsicClassDefs.toSeq.map { s =>
        "scala" -> parser.parseClasslike(s.tree.asInstanceOf[ClassDef])
      }
      topLevels ++= intrinsicClassDefs
      topLevels ++= intrinsicTypeDefs
      val scalaPckg = defn.ScalaPackage
      given parser.qctx.type = parser.qctx
      topLevels += "scala" -> Member(scalaPckg.fullName, scalaPckg.dri, Kind.Package)
      topLevels += mergeAnyRefAliasAndObject(parser)

  def result(): (List[Member], Option[Comment]) =
    topLevels.clear()
    rootDoc = None
    val filePaths = ctx.args.tastyFiles.map(_.getAbsolutePath).toList
    val classpath = ctx.args.classpath.split(java.io.File.pathSeparator).toList

    if filePaths.nonEmpty then inspectFilesInContext(classpath, filePaths)

    val all = topLevels.result()
    all.groupBy(_._1).map { case (pckName, members) =>
      val (pcks, rest) = members.map(_._2).partition(_.kind == Kind.Package)
      val basePck = pcks.reduce( (p1, p2) =>
        val withNewMembers = p1.withNewMembers(p2.members)
        if withNewMembers.docs.isEmpty then withNewMembers.withDocs(p2.docs) else withNewMembers
      )
      basePck.withMembers((basePck.members ++ rest).sortBy(_.name))
    }.toList -> rootDoc

  def mergeAnyRefAliasAndObject(parser: TastyParser) =
    import parser.qctx.reflect._
    val javaLangObjectDef = defn.ObjectClass.tree.asInstanceOf[ClassDef]
    val objectMembers = parser.extractPatchedMembers(javaLangObjectDef)
    val aM = parser.parseTypeDef(defn.AnyRefClass.tree.asInstanceOf[TypeDef])
    "scala" -> aM.copy(
      kind = Kind.Class(Nil, Nil),
      members = objectMembers
    )
/** Parses a single Tasty compilation unit. */
case class TastyParser(
  qctx: Quotes,
  inspector: ScaladocTastyInspector,
)(
  isSkipped: qctx.reflect.Symbol => Boolean
)(
  using val ctx: DocContext
) extends BasicSupport with TypesSupport with ClassLikeSupport with PackageSupport with InkuireSupport:
  import qctx.reflect._

  private given qctx.type = qctx

  val intrinsicClassDefs = Set(
    defn.AnyClass,
    defn.MatchableClass,
    defn.ScalaPackage.typeMember("AnyKind"),
    defn.AnyValClass,
    defn.NullClass,
    defn.NothingClass,
    defn.ScalaPackage.typeMember("Singleton"),
  )

  val noPosClassDefs = intrinsicClassDefs ++ Set(
    defn.ObjectClass,
    defn.AnyRefClass
  )

  val intrinsicTypeDefs = Set(
    defn.ScalaPackage.typeMember("&"),
    defn.ScalaPackage.typeMember("|"),
  )
  def processTree[T](tree: Tree)(op: => T): Option[T] = try Option(op) catch
    case e: Exception  =>
      report.warning(throwableToString(e), tree.pos)
      None
  def processTreeOpt[T](tree: Tree)(op: => Option[T]): Option[T] = try op catch
    case e: Exception =>
      report.warning(throwableToString(e), tree.pos)
      None

  def parseRootTree(root: Tree): Seq[(String, Member)] =
    val docs = Seq.newBuilder[(String, Member)]
    object Traverser extends TreeTraverser:
      var seen: List[Tree] = Nil

      override def traverseTree(tree: Tree)(owner: Symbol): Unit =
        seen = tree :: seen
        if !isSkipped(tree.symbol) then tree match {
          case pck: PackageClause =>
            docs += parsePackage(pck)
            super.traverseTree(tree)(owner)
          case packageObject: ClassDef if(packageObject.symbol.name.contains("package$")) =>
            docs += parsePackageObject(packageObject)
          case clazz: ClassDef if clazz.symbol.shouldDocumentClasslike =>
            docs += clazz.symbol.packageName -> parseClasslike(clazz)
          case _ =>
        }
        seen = seen.tail

    try Traverser.traverseTree(root)(Symbol.spliceOwner)
    catch case e: Throwable =>
      println(s"Problem parsing ${root.pos}, documentation may not be generated.")
      e.printStackTrace()

    docs.result()
