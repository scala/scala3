package dotty.tools
package dotc
package semanticdb

import core._
import Phases._
import ast.tpd._
import Contexts._
import Symbols._
import Flags._
import Names.Name
import StdNames.nme
import util.Spans.Span
import util.{SourceFile, SourcePosition}
import collection.mutable
import java.lang.Character.{isJavaIdentifierPart, isJavaIdentifierStart}
import java.nio.file.Paths

import ast.untpd.given
import NameOps.given

import scala.annotation.{ threadUnsafe => tu, tailrec }

/** Extract symbol references and uses to semanticdb files.
 *  See https://scalameta.org/docs/semanticdb/specification.html#symbol-1
 *  for a description of the format.
 *  TODO: Also extract type information
 */
class ExtractSemanticDB extends Phase with
  import Scala.{_, given}
  import Symbols.given

  override val phaseName: String = ExtractSemanticDB.name

  override def isRunnable(implicit ctx: Context) =
    super.isRunnable && ctx.settings.Ysemanticdb.value

  // Check not needed since it does not transform trees
  override def isCheckable: Boolean = false

  override def run(implicit ctx: Context): Unit =
    val unit = ctx.compilationUnit
    val extract = Extractor()
    extract.traverse(unit.tpdTree)
    ExtractSemanticDB.write(unit.source, extract.occurrences.toList, extract.symbols.toList)

  /** Extractor of symbol occurrences from trees */
  class Extractor extends TreeTraverser with

    private var nextLocalIdx: Int = 0

    /** The index of a local symbol */
    private val locals = mutable.HashMap[Symbol, Int]()

    /** The local symbol(s) starting at given offset */
    private val symsAtOffset = new mutable.HashMap[Int, Set[Symbol]]() with
      override def default(key: Int) = Set[Symbol]()

    /** The extracted symbol occurrences */
    val occurrences = new mutable.ListBuffer[SymbolOccurrence]()

    /** The extracted symbol infos */
    val symbols = new mutable.HashSet[SymbolInformation]()

    /** The symbol occurrences generated so far, as a set */
    private val generated = new mutable.HashSet[SymbolOccurrence]

    /** Add semanticdb name of the given symbol to string builder */
    private def addSymName(b: StringBuilder, sym: Symbol)(given ctx: Context): Unit =

      def isJavaIdent(str: String) =
        isJavaIdentifierStart(str.head) && str.tail.forall(isJavaIdentifierPart)

      def addName(name: Name) =
        val str = name.toString.unescapeUnicode
        if isJavaIdent(str) then b append str
        else b append '`' append str append '`'

      /** Is symbol global? Non-global symbols get localX names */
      def isGlobal(sym: Symbol): Boolean =
        sym.is(Package)
        || !sym.isSelfSym && (sym.is(Param) || sym.owner.isClass) && isGlobal(sym.owner)

      def addOwner(owner: Symbol): Unit =
        if !owner.isRoot then addSymName(b, owner)

      def addOverloadIdx(sym: Symbol): Unit =
        val decls =
          val decls0 = sym.owner.info.decls.lookupAll(sym.name)
          if sym.owner.isAllOf(JavaModule)
            decls0 ++ sym.owner.companionClass.info.decls.lookupAll(sym.name)
          else
            decls0
        end decls
        val alts = decls.filter(_.is(Method)).toList.reverse
        alts match
        case notSym :: rest if sym != notSym =>
          val idx = rest.indexOf(sym).ensuring(_ >= 0)
          b.append('+').append(idx + 1)
        case _ =>

      def addDescriptor(sym: Symbol): Unit =
        if sym.is(ModuleClass) then
          addDescriptor(sym.sourceModule)
        else if sym.is(TypeParam) then
          b.append('['); addName(sym.name); b.append(']')
        else if sym.is(Param) then
          b.append('('); addName(sym.name); b.append(')')
        else if sym.isRoot then
          b.append(Symbols.RootPackage)
        else if sym.isEmptyPackage then
          b.append(Symbols.EmptyPackage)
        else if (sym.isScala2PackageObject) then
          b.append(Symbols.PackageObjectDescriptor)
        else
          addName(sym.name)
          if sym.is(Package) then b.append('/')
          else if sym.isType || sym.isAllOf(JavaModule) then b.append('#')
          else if sym.isOneOf(Method | Mutable)
          && (!sym.is(StableRealizable) || sym.isConstructor) then
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
        if isGlobal(sym) then
          addOwner(sym.owner); addDescriptor(sym)
        else
          b.append(Symbols.LocalPrefix).append(localIdx(sym))

    end addSymName

    /** The semanticdb name of the given symbol */
    private def symbolName(sym: Symbol)(given ctx: Context): String =
      val b = StringBuilder(20)
      addSymName(b, sym)
      b.toString

    inline private def source(given ctx: Context) = ctx.compilationUnit.source

    private def range(span: Span)(given ctx: Context): Option[Range] =
      def lineCol(offset: Int) = (source.offsetToLine(offset), source.column(offset))
      val (startLine, startCol) = lineCol(span.start)
      val (endLine, endCol) = lineCol(span.end)
      Some(Range(startLine, startCol, endLine, endCol))

    /** Definitions of this symbol should be excluded from semanticdb */
    private def excludeDef(sym: Symbol)(given Context): Boolean =
      !sym.exists
      || sym.isLocalDummy
      || sym.is(Synthetic)
      || sym.owner.is(Synthetic) && !sym.isAllOf(EnumCase)
      || sym.isConstructor && sym.owner.is(ModuleClass)
      || sym.isAnonymous
      || excludeDefStrict(sym)

    private def excludeDefStrict(sym: Symbol)(given Context): Boolean =
      sym.name.is(NameKinds.DefaultGetterName)
      || sym.name.isWildcard

    /** Uses of this symbol where the reference has given span should be excluded from semanticdb */
    private def excludeUseStrict(sym: Symbol, span: Span)(given Context): Boolean =
      excludeDefStrict(sym)
      || excludeDef(sym) && span.zeroLength

    private def symbolKind(sym: Symbol, symkinds: Set[SymbolKind])(given Context): SymbolInformation.Kind =
      if sym.isTypeParam
        SymbolInformation.Kind.TYPE_PARAMETER
      else if sym.is(TermParam)
        SymbolInformation.Kind.PARAMETER
      else if sym.isTerm && sym.owner.isTerm
        SymbolInformation.Kind.LOCAL
      else if sym.isInlineMethod || sym.is(Macro)
        SymbolInformation.Kind.MACRO
      else if sym.isConstructor
        SymbolInformation.Kind.CONSTRUCTOR
      else if sym.isSelfSym
        SymbolInformation.Kind.SELF_PARAMETER
      else if sym.isOneOf(Method) || symkinds.exists(_.isVarOrVal)
        SymbolInformation.Kind.METHOD
      else if sym.isPackageObject
        SymbolInformation.Kind.PACKAGE_OBJECT
      else if sym.is(Module)
        SymbolInformation.Kind.OBJECT
      else if sym.is(Package)
        SymbolInformation.Kind.PACKAGE
      else if sym.isAllOf(JavaInterface)
        SymbolInformation.Kind.INTERFACE
      else if sym.is(Trait)
        SymbolInformation.Kind.TRAIT
      else if sym.isClass
        SymbolInformation.Kind.CLASS
      else if sym.isType
        SymbolInformation.Kind.TYPE
      else if sym.is(ParamAccessor)
        SymbolInformation.Kind.FIELD
      else
        SymbolInformation.Kind.UNKNOWN_KIND

    private def symbolProps(sym: Symbol, symkinds: Set[SymbolKind])(given Context): Int =
      var props = 0
      if sym.isPrimaryConstructor
        props |= SymbolInformation.Property.PRIMARY.value
      if sym.is(Abstract) || symkinds.contains(SymbolKind.Abstract)
        props |= SymbolInformation.Property.ABSTRACT.value
      if sym.is(Final)
        props |= SymbolInformation.Property.FINAL.value
      if sym.is(Sealed)
        props |= SymbolInformation.Property.SEALED.value
      if sym.isOneOf(GivenOrImplicit)
        props |= SymbolInformation.Property.IMPLICIT.value
      if sym.is(Lazy)
        props |= SymbolInformation.Property.LAZY.value
      if sym.isAllOf(CaseClass) || sym.isAllOf(EnumCase)
        props |= SymbolInformation.Property.CASE.value
      if sym.is(Covariant)
        props |= SymbolInformation.Property.COVARIANT.value
      if sym.is(Contravariant)
        props |= SymbolInformation.Property.CONTRAVARIANT.value
      if sym.isAllOf(DefaultMethod | JavaDefined) || sym.is(Accessor) && sym.name.is(NameKinds.DefaultGetterName)
        props |= SymbolInformation.Property.DEFAULT.value
      if symkinds.exists(_.isVal)
        props |= SymbolInformation.Property.VAL.value
      if symkinds.exists(_.isVar)
        props |= SymbolInformation.Property.VAR.value
      if sym.is(JavaStatic)
        props |= SymbolInformation.Property.STATIC.value
      if sym.is(Enum)
        props |= SymbolInformation.Property.ENUM.value
      props

    private def symbolInfo(sym: Symbol, symbolName: String, symkinds: Set[SymbolKind])(given Context): SymbolInformation =
      SymbolInformation(
        symbol = symbolName,
        language = Language.SCALA,
        kind = symbolKind(sym, symkinds),
        properties = symbolProps(sym, symkinds),
        displayName = Symbols.displaySymbol(sym)
      )

    private def registerSymbol(sym: Symbol, symbolName: String, symkinds: Set[SymbolKind])(given Context): Unit =
      symbols += symbolInfo(sym, symbolName, symkinds)

    private def registerOccurrence(symbol: String, span: Span, role: SymbolOccurrence.Role)(given Context): Unit =
      val occ = SymbolOccurrence(symbol, range(span), role)
      if !generated.contains(occ) && occ.symbol.nonEmpty then
        occurrences += occ
        generated += occ

    private def registerUse(sym: Symbol, span: Span)(given Context) =
      if !excludeUseStrict(sym, span) then
        registerOccurrence(symbolName(sym), span, SymbolOccurrence.Role.REFERENCE)

    private def registerDefinition(sym: Symbol, span: Span, symkinds: Set[SymbolKind])(given Context) =
      val symbol = symbolName(sym)
      registerOccurrence(symbol, span, SymbolOccurrence.Role.DEFINITION)
      if !sym.is(Package)
        registerSymbol(sym, symbol, symkinds)

    private def spanOfSymbol(sym: Symbol, span: Span)(given Context): Span =
      val contents = if source.exists then source.content() else Array.empty[Char]
      val idx = contents.indexOfSlice(sym.name.show, span.start)
      val start = if idx >= 0 then idx else span.start
      Span(start, start + sym.name.show.length, start)

    private inline def (list: List[List[ValDef]]) isSingleArg = list match
      case (_::Nil)::Nil => true
      case _             => false

    private def (tree: DefDef) isSetterDef(given Context): Boolean =
      tree.name.isSetterName && tree.mods.is(Accessor) && tree.vparamss.isSingleArg

    private def findGetters(ctorParams: Set[Names.TermName], body: List[Tree])(given Context): Map[Names.TermName, ValDef] =
      if ctorParams.isEmpty || body.isEmpty then
        Map.empty
      else
        body.collect({
          case tree: ValDef
          if ctorParams.contains(tree.name)
          && !tree.symbol.isPrivate =>
            tree.name -> tree
        }).toMap
    end findGetters

    private def adjustSpanToName(span: Span, qualSpan: Span, name: Name)(given Context) =
      val end = span.end
      val limit = qualSpan.end
      val start =
        if limit < end then
          val len = name.toString.length
          if source.content()(end - 1) == '`' then end - len - 2 else end - len
        else limit
      Span(start max limit, end)

    private given spanOps: (span: Span) with
      def hasLength: Boolean = span.start != span.end
      def zeroLength: Boolean = span.start == span.end

    /**Consume head while not an import statement.
     * Returns the rest of the list after the first import, or else the empty list
     */
    @tailrec
    private def (body: List[Tree]) foreachUntilImport(op: Tree => Unit): List[Tree] = body match
      case ((_: Import) :: rest) => rest
      case stat :: rest =>
        op(stat)
        rest.foreachUntilImport(op)
      case Nil => Nil

    private def (sym: Symbol) adjustIfCtorTyparam(given Context) =
      if sym.isType && sym.owner.isConstructor
        matchingMemberType(sym, sym.owner.owner)
      else
        sym

    private inline def matchingMemberType(ctorTypeParam: Symbol, classSym: Symbol)(given Context) =
      classSym.info.member(ctorTypeParam.name).symbol

    /**Necessary because not all of the eventual flags are propagated from the Tree to the symbol yet.
     */
    private def symbolKinds(tree: NamedDefTree)(given Context): Set[SymbolKind] =
      if tree.symbol.isSelfSym
        Set.empty
      else
        val symkinds = mutable.HashSet.empty[SymbolKind]
        tree match
        case tree: ValDef =>
          if !tree.symbol.owner.is(Method)
            symkinds += (if tree.mods is Mutable then SymbolKind.Var else SymbolKind.Val)
          if tree.rhs.isEmpty && !tree.symbol.isOneOf(TermParam | CaseAccessor | ParamAccessor)
            symkinds += SymbolKind.Abstract
        case tree: DefDef =>
          if tree.isSetterDef
            symkinds += SymbolKind.Setter
          else if tree.rhs.isEmpty
            symkinds += SymbolKind.Abstract
        case tree: Bind => symkinds += SymbolKind.Val
        case _ =>
        symkinds.toSet

    private inline def ctorParams(
      vparamss: List[List[ValDef]], body: List[Tree])(traverseTpt: => Tree => Unit)(given Context): Unit =
      @tu lazy val getters = findGetters(vparamss.flatMap(_.map(_.name)).toSet, body)
      for
        vparams <- vparamss
        vparam  <- vparams
      do
        if !vparam.name.isWildcard
          val symkinds =
            getters.get(vparam.name).fold(SymbolKind.emptySet)(getter =>
              if getter.mods.is(Mutable) then SymbolKind.VarSet else SymbolKind.ValSet)
          registerSymbol(vparam.symbol, symbolName(vparam.symbol), symkinds)
        traverseTpt(vparam.tpt)

    override def traverse(tree: Tree)(given Context): Unit =

      inline def traverseCtorParamTpt(ctorSym: Symbol, tpt: Tree): Unit =
        val tptSym = tpt.symbol
        if tptSym.owner == ctorSym
          val found = matchingMemberType(tptSym, ctorSym.owner)
          if !excludeUseStrict(found, tpt.span) && tpt.span.hasLength
            registerUse(found, tpt.span)
        else
          traverse(tpt)

      for annot <- tree.symbol.annotations do
        if annot.tree.span.exists
          && annot.symbol.owner != defn.ScalaAnnotationInternal
        then
          traverse(annot.tree)

      tree match
        case tree: PackageDef =>
          if !excludeDef(tree.pid.symbol) && tree.pid.span.hasLength
            tree.pid match
            case tree @ Select(qual, name) =>
              registerDefinition(tree.symbol, adjustSpanToName(tree.span, qual.span, name), Set.empty)
              traverse(qual)
            case tree => registerDefinition(tree.symbol, tree.span, Set.empty)
          tree.stats.foreach(traverse)
        case tree: ValDef if tree.symbol.is(Module) => // skip module val
        case tree: NamedDefTree
        if !excludeDef(tree.symbol) && tree.span.hasLength =>
          registerDefinition(tree.symbol, tree.nameSpan, symbolKinds(tree))
          val privateWithin = tree.symbol.privateWithin
          if privateWithin.exists
            registerUse(privateWithin, spanOfSymbol(privateWithin, tree.span))
          tree match
          case tree: ValDef if tree.symbol.isAllOf(EnumValue) =>
            tree.rhs match
            case Block(TypeDef(_, template: Template) :: _, _) => // simple case with specialised extends clause
              template.parents.foreach(traverse)
            case _ => // calls $new
          case tree: ValDef if tree.symbol.isSelfSym =>
            if tree.tpt.span.hasLength
              traverse(tree.tpt)
          case tree: DefDef if tree.symbol.isConstructor => // ignore typeparams for secondary ctors
            tree.vparamss.foreach(_.foreach(traverse))
            traverse(tree.rhs)
          case _ => traverseChildren(tree)
        case tree: (ValDef|DefDef|TypeDef) if tree.symbol.is(Synthetic, butNot=Module) && !tree.symbol.isAnonymous => // skip
        case tree: Template =>
          val ctorSym = tree.constr.symbol
          if !excludeDef(ctorSym)
            registerDefinition(ctorSym, tree.constr.span, Set.empty)
            ctorParams(tree.constr.vparamss, tree.body)(traverseCtorParamTpt(ctorSym, _))
          for parent <- tree.parentsOrDerived do
            if
              parent.symbol != defn.ObjectClass.primaryConstructor
              && parent.tpe.dealias != defn.SerializableType
              && parent.symbol != defn.ProductClass
            then
              traverse(parent)
          val selfSpan = tree.self.span
          if selfSpan.exists && selfSpan.hasLength then
            traverse(tree.self)
          if tree.symbol.owner.is(Enum, butNot=Case)
            tree.body.foreachUntilImport(traverse).foreach(traverse) // the first import statement
          else
            tree.body.foreach(traverse)
        case tree: Assign =>
          if !excludeUseStrict(tree.lhs.symbol, tree.lhs.span)
            val lhs = tree.lhs.symbol
            val setter = lhs.matchingSetter.orElse(lhs)
            tree.lhs match
              case tree @ Select(qual, name) => registerUse(setter, adjustSpanToName(tree.span, qual.span, name))
              case tree                      => registerUse(setter, tree.span)
            traverseChildren(tree.lhs)
          traverse(tree.rhs)
        case tree: Ident =>
          if tree.name != nme.WILDCARD then
            val sym = tree.symbol.adjustIfCtorTyparam
            if !excludeUseStrict(sym, tree.span) then
              registerUse(sym, tree.span)
        case tree: Select =>
          val qualSpan = tree.qualifier.span
          val sym = tree.symbol.adjustIfCtorTyparam
          if !excludeUseStrict(sym, tree.span) then
            registerUse(sym, adjustSpanToName(tree.span, qualSpan, tree.name))
          if qualSpan.exists && qualSpan.hasLength then
            traverseChildren(tree)
        case tree: Import =>
          if tree.span.exists && tree.span.hasLength then
            for sel <- tree.selectors do
              val imported = sel.imported.name
              if imported != nme.WILDCARD then
                for alt <- tree.expr.tpe.member(imported).alternatives do
                  registerUse(alt.symbol, sel.imported.span)
                  if (alt.symbol.companionClass.exists)
                    registerUse(alt.symbol.companionClass, sel.imported.span)
            traverseChildren(tree)
        case tree: Inlined =>
          traverse(tree.call)
        case tree: Annotated => // skip the annotation (see `@param` in https://github.com/scalameta/scalameta/blob/633824474e99bbfefe12ad0cc73da1fe064b3e9b/tests/jvm/src/test/resources/example/Annotations.scala#L37)
          traverse(tree.arg)
        case _ =>
          traverseChildren(tree)

object ExtractSemanticDB with
  import java.nio.file.Path
  import scala.collection.JavaConverters._
  import java.nio.file.Files

  val name: String = "extractSemanticDB"

  def write(source: SourceFile, occurrences: List[SymbolOccurrence], symbols: List[SymbolInformation])(given ctx: Context): Unit =
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
    val outpath = targetRoot
      .resolve("META-INF")
      .resolve("semanticdb")
      .resolve(relPath)
      .resolveSibling(sourcePath.getFileName().toString() + ".semanticdb")
    Files.createDirectories(outpath.getParent())
    val doc: TextDocument = TextDocument(
      schema = Schema.SEMANTICDB4,
      language = Language.SCALA,
      uri = relPath.toString,
      text = "",
      md5 = internal.MD5.compute(String(source.content)),
      symbols = symbols,
      occurrences = occurrences
    )
    val docs = TextDocuments(List(doc))
    val out = Files.newOutputStream(outpath)
    try
      val stream = internal.SemanticdbOutputStream.newInstance(out)
      docs.writeTo(stream)
      stream.flush()
    finally
      out.close()
