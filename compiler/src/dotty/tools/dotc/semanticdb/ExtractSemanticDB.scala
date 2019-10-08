package dotty.tools
package dotc
package semanticdb

import core._
import Phases._
import ast.Trees._
import ast.untpd
import Contexts._
import Symbols._
import Flags._
import Decorators._
import Names.Name
import StdNames.nme
import util.{SourceFile, SourcePosition}
import collection.mutable
import java.lang.Character.{isJavaIdentifierPart, isJavaIdentifierStart}
import java.nio.file.Paths

class ExtractSemanticDB extends Phase {
  import ast.tpd._

  override val phaseName: String = ExtractSemanticDB.name

  override def isRunnable(implicit ctx: Context) =
    super.isRunnable && ctx.settings.Ysemanticdb.value

  // Check not needed since it does not transform trees
  override def isCheckable: Boolean = false

  //def genInfo(unit: CompilationUnit, occurrences: List[SymbolOccurrence])

  override def run(implicit ctx: Context): Unit = {
    val extract = Extractor()
    val unit = ctx.compilationUnit
    extract.traverse(unit.tpdTree)
    ExtractSemanticDB.write(unit.source, extract.occurrences.toList)
  }

  class Extractor extends TreeTraverser {

    private val locals = mutable.HashMap[Symbol, Int]()

    val occurrences = new mutable.ListBuffer[SymbolOccurrence]()

    val localIndex = mutable.HashMap[Int, Int]()

    private var myLocalIdx: Int = -1
    private def nextLocalIdx() =
      myLocalIdx += 1
      myLocalIdx

    private def symbolName(sym: Symbol)(given ctx: Context): String =

      def isJavaIdent(str: String) =
        isJavaIdentifierStart(str.head) && str.tail.forall(isJavaIdentifierPart)

      def nameToString(name: Name) =
        val str = name.toString
        if isJavaIdent(str) then str else "`" + str + "`"

      def isGlobal(sym: Symbol): Boolean =
        sym.is(Package)
        || (sym.is(Param) || sym.owner.isClass) && isGlobal(sym.owner)

      def ownerString(owner: Symbol): String =
        if owner.isRoot || owner.isEmptyPackage then "" else symbolName(owner)

      def overloadIdx(sym: Symbol): String =
        val alts = sym.owner.info.decls.lookupAll(sym.name).toList
        if alts.tail.isEmpty then ""
        else
          val idx = alts.indexOf(sym)
          assert(idx >= 0)
          "+" + idx.toString

      def descriptor(sym: Symbol): String =
        if sym.is(ModuleClass) then
          descriptor(sym.sourceModule)
        else
          val str = nameToString(sym.name)
          if sym.is(Package) then str + "/"
          else if sym.isType then str + "#"
          else if sym.isRealMethod then str + "(" + overloadIdx(sym) + ")"
          else if sym.is(TermParam) || sym.is(ParamAccessor) then "(" + str + ")"
          else if sym.is(TypeParam) then "[" + str + "]"
          else if sym.isTerm then str + "."
          else throw new AssertionError(i"unhandled symbol: $sym: ${sym.info} with ${sym.flagsString}")

      def localIdx(sym: Symbol)(given Context): Int =
        localIndex.getOrElseUpdate(sym.span.start, nextLocalIdx())

      if sym.isRoot then "_root_"
      else if sym.isEmptyPackage then "_empty_"
      else if isGlobal(sym) then ownerString(sym.owner) + descriptor(sym)
      else "local" + localIdx(sym)
    end symbolName

    private def range(pos: SourcePosition)(given Context): Option[Range] =
      val src = pos.source
      def lineCol(offset: Int) = (src.offsetToLine(offset), src.column(offset))
      val (startLine, startCol) = lineCol(pos.span.start)
      val (endLine, endCol) = lineCol(pos.span.end)
      Some(Range(startLine, startCol, endLine, endCol))

    private def excluded(sym: Symbol)(given Context): Boolean =
      !sym.exists || sym.isLocalDummy

    private def registerOccurrence(sym: Symbol, pos: SourcePosition, role: SymbolOccurrence.Role)(given Context): Unit =
      if !excluded(sym) then
        //println(i"register: ${symbolName(sym)}")
        occurrences += SymbolOccurrence(symbolName(sym), range(pos), role)

    private def registerUse(sym: Symbol, pos: SourcePosition)(given Context) =
      registerOccurrence(sym, pos, SymbolOccurrence.Role.REFERENCE)
    private def registerDef(sym: Symbol, pos: SourcePosition)(given Context) =
      registerOccurrence(sym, pos, SymbolOccurrence.Role.DEFINITION)

    override def traverse(tree: Tree)(given ctx: Context): Unit =
      tree match
        case tree: DefTree =>
          registerDef(tree.symbol, tree.sourcePos)
          traverseChildren(tree)
        case tree: RefTree =>
          registerUse(tree.symbol, tree.sourcePos)
          traverseChildren(tree)
        case tree: Import =>
          for sel <- tree.selectors do
            val imported = sel.imported.name
            if imported != nme.WILDCARD then
              for alt <- tree.expr.tpe.member(imported).alternatives do
                registerUse(alt.symbol, sel.imported.sourcePos)
        case _ =>
          traverseChildren(tree)
  }
}

object ExtractSemanticDB {
  import java.nio.file.Path
  import scala.collection.JavaConverters._
  import java.nio.file.Files

  val name: String = "extractSemanticDB"

  def write(source: SourceFile, occurrences: List[SymbolOccurrence])(given ctx: Context): Unit =
    val sourcePath = source.file.jpath
    val sourceRoot = Paths.get(ctx.settings.sourceroot.value)
    val targetRoot =
      val targetRootSetting = ctx.settings.targetroot.value
      if targetRootSetting.isEmpty then ctx.settings.outputDir.value.jpath
      else Paths.get(targetRootSetting)
    println(i"extract from $sourcePath from $sourceRoot, targetRoot = $targetRoot")
    val relPath = sourceRoot.relativize(sourcePath)
    println(i"relPath = $relPath")
    val relURI = relPath.iterator().asScala.mkString("/")
    val outpath = targetRoot
      .resolve("META-INF")
      .resolve("semanticdb")
      .resolve(relPath)
      .resolveSibling(sourcePath.getFileName().toString() + ".semanticdb")
    Files.createDirectories(outpath.getParent())
    val doc: TextDocument = TextDocument(
      schema = Schema.SEMANTICDB4,
      language = Language.SCALA,
      uri = relURI,
      md5 = MD5.compute(String(source.content)),
      occurrences = occurrences
    )
    val docs = TextDocuments(List(doc))
    val out = Files.newOutputStream(outpath)
    try
      val stream = SemanticdbOutputStream.newInstance(out)
      docs.writeTo(stream)
      stream.flush()
    finally
      out.close()
}
