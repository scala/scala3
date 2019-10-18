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
import util.Spans.Span
import util.{SourceFile, SourcePosition}
import collection.mutable
import java.lang.Character.{isJavaIdentifierPart, isJavaIdentifierStart}
import java.nio.file.Paths

/** Extract symbol references and uses to semanticdb files.
 *  See https://scalameta.org/docs/semanticdb/specification.html#symbol-1
 *  for a description of the format.
 *  TODO: Also extract type information
 */
class ExtractSemanticDB extends Phase {
  import ast.tpd._

  override val phaseName: String = ExtractSemanticDB.name

  override def isRunnable(implicit ctx: Context) =
    super.isRunnable && ctx.settings.Ysemanticdb.value

  // Check not needed since it does not transform trees
  override def isCheckable: Boolean = false

  override def run(implicit ctx: Context): Unit =
    val unit = ctx.compilationUnit
    val extract = Extractor()
    extract.traverse(unit.tpdTree)
    ExtractSemanticDB.write(unit.source, extract.occurrences.toList)

  /** Extractor of symbol occurrences from trees */
  class Extractor extends TreeTraverser {

    private var nextLocalIdx: Int = 0

    /** The index of a local symbol */
    private val locals = mutable.HashMap[Symbol, Int]()

    /** The local symbol(s) starting at given offset */
    private val symsAtOffset = new mutable.HashMap[Int, Set[Symbol]]() {
      override def default(key: Int) = Set[Symbol]()
    }

    /** The extracted symbol occurrences */
    val occurrences = new mutable.ListBuffer[SymbolOccurrence]()

    /** The symbol occurrences generated so far, as a set */
    private val generated = new mutable.HashSet[SymbolOccurrence]

    /** Add semanticdb name of the given symbol to string builder */
    private def addSymName(b: StringBuilder, sym: Symbol)(given ctx: Context): Unit =

      def isJavaIdent(str: String) =
        isJavaIdentifierStart(str.head) && str.tail.forall(isJavaIdentifierPart)

      def addName(name: Name) =
        val str = name.toString
        if isJavaIdent(str) then b.append(str)
        else b.append('`').append(str).append('`')

      /** Is symbol global? Non-global symbols get localX names */
      def isGlobal(sym: Symbol): Boolean =
        sym.is(Package)
        || !sym.isSelfSym && (sym.is(Param) || sym.owner.isClass) && isGlobal(sym.owner)

      def addOwner(owner: Symbol): Unit =
        if !owner.isRoot && !owner.isEmptyPackage then addSymName(b, owner)

      def addOverloadIdx(sym: Symbol): Unit =
        val alts = sym.owner.info.decls.lookupAll(sym.name).toList.reverse
        if alts.tail.nonEmpty then
          val idx = alts.indexOf(sym)
          assert(idx >= 0)
          if idx > 0 then
            b.append('+').append(idx)

      def addDescriptor(sym: Symbol): Unit =
        if sym.is(ModuleClass) then
          addDescriptor(sym.sourceModule)
        else if sym.is(TypeParam) then
          b.append('['); addName(sym.name); b.append(']')
        else if sym.is(Param) || sym.is(ParamAccessor) then
          b.append('('); addName(sym.name); b.append(')')
        else
          addName(sym.name)
          if sym.is(Package) then b.append('/')
          else if sym.isType then b.append('#')
          else if sym.is(Method) && (!sym.is(Accessor) || sym.is(Mutable)) then
            b.append('('); addOverloadIdx(sym); b.append(").")
          else b.append('.')

      /** The index of local symbol `sym`. Symbols with the same name and
       *  the same starting position have the same index.
       */
      def localIdx(sym: Symbol)(given Context): Int =
        def computeLocalIdx(): Int =
          symsAtOffset(sym.span.start).find(_.name == sym.name) match
            case Some(other) => localIdx(other)
            case None =>
              val idx = nextLocalIdx
              nextLocalIdx += 1
              locals(sym) = idx
              symsAtOffset(sym.span.start) += sym
              idx
        locals.getOrElseUpdate(sym, computeLocalIdx())

      if sym.exists then
        if sym.isRoot then
          b.append("_root_")
        else if sym.isEmptyPackage then
          b.append("_empty_")
        else if isGlobal(sym) then
          addOwner(sym.owner); addDescriptor(sym)
        else
          b.append("local").append(localIdx(sym))
    end addSymName

    /** The semanticdb name of the given symbol */
    private def symbolName(sym: Symbol)(given ctx: Context): String =
      val b = StringBuilder()
      addSymName(b, sym)
      b.toString

    private def source(given ctx: Context) = ctx.compilationUnit.source

    private def range(span: Span)(given ctx: Context): Option[Range] =
      def lineCol(offset: Int) = (source.offsetToLine(offset), source.column(offset))
      val (startLine, startCol) = lineCol(span.start)
      val (endLine, endCol) = lineCol(span.end)
      Some(Range(startLine, startCol, endLine, endCol))

    private val WILDCARDTypeName = nme.WILDCARD.toTypeName

    private def isWildcard(name: Name)(given ctx: Context) = name match
      case nme.WILDCARD | WILDCARDTypeName           => true
      case _ if name.is(NameKinds.WildcardParamName) => true
      case _                                         => false

    /** Definitions of this symbol should be excluded from semanticdb */
    private def excludeDef(sym: Symbol)(given Context): Boolean =
      !sym.exists || sym.isLocalDummy || sym.is(Synthetic)

    /** Uses of this symbol where the reference has given span should be excluded from semanticdb */
    private def excludeUse(sym: Symbol, span: Span)(given Context): Boolean =
      excludeDef(sym) && span.start == span.end

    private def registerOccurrence(sym: Symbol, span: Span, role: SymbolOccurrence.Role)(given Context): Unit =
      val occ = SymbolOccurrence(symbolName(sym), range(span), role)
      if !generated.contains(occ) then
        occurrences += occ
        generated += occ

    private def registerUse(sym: Symbol, span: Span)(given Context) =
      if !excludeUse(sym, span) && !isWildcard(sym.name) then
        registerOccurrence(sym, span, SymbolOccurrence.Role.REFERENCE)

    private def registerDefinition(sym: Symbol, span: Span)(given Context) =
      if !isWildcard(sym.name) then
        registerOccurrence(sym, span, SymbolOccurrence.Role.DEFINITION)

    override def traverse(tree: Tree)(given ctx: Context): Unit =
      def registerPath(expr: Tree): Unit = expr match
        case t @ Select(expr, _) =>
          registerUse(t.symbol, t.span)
          registerPath(expr)

        case _ =>

      for annot <- tree.symbol.annotations do
        if annot.tree.span.exists
          && annot.symbol.owner != defn.ScalaAnnotationInternal
        then
          traverse(annot.tree)

      tree match
        case tree: ValDef if tree.symbol.is(Module) => // skip module val
        case tree: NamedDefTree
        if !excludeDef(tree.symbol) && tree.span.start != tree.span.end =>
          registerDefinition(tree.symbol, tree.nameSpan)
          traverseChildren(tree)
        case tree: Ident =>
          if tree.name != nme.WILDCARD && !excludeUse(tree.symbol, tree.span) then
            registerUse(tree.symbol, tree.span)
        case tree: Select =>
          if !excludeUse(tree.symbol, tree.span) then
            val end = tree.span.end
            val limit = tree.qualifier.span.end
            val start =
              if limit < end then
                val len = tree.name.toString.length
                if source.content()(end - 1) == '`' then end - len - 1 else end - len
              else limit
            registerUse(tree.symbol, Span(start max limit, end))
          traverseChildren(tree)
        case tree: Import =>
          for sel <- tree.selectors do
            val imported = sel.imported.name
            if imported != nme.WILDCARD then
              for alt <- tree.expr.tpe.member(imported).alternatives do
                registerUse(alt.symbol, sel.imported.span)
              registerPath(tree.expr)
        case tree: Inlined =>
          traverse(tree.call)
        case tree: PackageDef => tree.stats.foreach(traverse)
        case tree: Annotated => // skip the annotation (see `@param` in https://github.com/scalameta/scalameta/blob/633824474e99bbfefe12ad0cc73da1fe064b3e9b/tests/jvm/src/test/resources/example/Annotations.scala#L37)
          traverse(tree.arg)
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
    def absolutePath(path: Path): Path = path.toAbsolutePath.normalize
    val sourcePath = absolutePath(source.file.jpath)
    val sourceRoot = absolutePath(Paths.get(ctx.settings.sourceroot.value))
    val targetRoot =
      val targetRootSetting = ctx.settings.targetroot.value
      absolutePath(
        if targetRootSetting.isEmpty then ctx.settings.outputDir.value.jpath
        else Paths.get(targetRootSetting)
      )
    val relPath = sourceRoot.relativize(sourcePath)
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
      text = "",
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
