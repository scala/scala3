package dotty.tools
package dotc
package semanticdb

import core.Contexts.*
import core.Symbols.*
import core.Flags.*
import core.MissingType
import core.NameOps.*
import core.Names.*
import core.StdNames.tpnme
import Scala3.{*, given}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.util.control.NonFatal

// The API here is a little tricky because it's used both as a stateful builder with local symbols,
// and for non-local symbols that do not need any state through the companion object.

private[semanticdb] class SemanticSymbolBuilder:
  import SemanticSymbolBuilder.*

  private var nextLocalIdx: Int = 0

  /** The index of a local symbol */
  private val locals = mutable.HashMap[Symbol, Int]()

  /** The local symbol(s) starting at given offset */
  private val symsAtOffset = new mutable.HashMap[Int, Set[Symbol]]().withDefault(_ => Set[Symbol]())

  def symbolName(sym: Symbol)(using Context): String =
    val b = StringBuilder(20)
    addSymName(b, sym, Some(this))
    b.toString

  def symbolName(sym: FakeSymbol)(using Context): String =
    val b = StringBuilder(20)
    sym match
      case sym: WildcardTypeSymbol =>
        addSymName(b, sym.owner, Some(this))
        b.append('[')
        addName(b, sym.name)
        b.append(']')
      case sym: TermParamRefSymbol =>
        addSymName(b, sym.owner, Some(this))
        b.append('(')
        addName(b, sym.name)
        b.append(')')
      case sym: TypeParamRefSymbol =>
        addSymName(b, sym.owner, Some(this))
        b.append('[')
        addName(b, sym.name)
        b.append(']')
      case sym: RefinementSymbol =>
        addLocalSymName(b)
    b.toString

  def funParamSymbol(sym: Symbol)(using Context): Name => String =
    if sym.isGlobal then
      val funSymbol = symbolName(sym)
      name => s"$funSymbol($name)"
    else
      name => locals.keys.find(local => local.isTerm && local.owner == sym && local.name == name)
                    .fold("<?>")(Symbols.LocalPrefix + locals(_))

  private def addLocalSymName(b: StringBuilder): Unit =
    val idx = nextLocalIdx
    nextLocalIdx += 1
    b.append(Symbols.LocalPrefix).append(idx)


  /** The index of local symbol `sym`. Symbols with the same name and
   * the same starting position have the same index.
   */
  private def localIdx(sym: Symbol)(using Context): Int =
    val startPos =
      // assert(sym.span.exists, s"$sym should have a span")
      if sym.span.exists then Some(sym.span.start) else None

    @tailrec
    def computeLocalIdx(sym: Symbol): Int = locals.get(sym) match
      case Some(idx) => idx
      case None =>
        (for {
          pos <- startPos
          syms <- symsAtOffset.get(pos)
          found <- syms.find(_.name == sym.name)
        } yield found) match
          case Some(other) => computeLocalIdx(other)
          case None =>
            val idx = nextLocalIdx
            nextLocalIdx += 1
            locals(sym) = idx
            startPos.foreach(pos => symsAtOffset(pos) += sym)
            idx
    end computeLocalIdx

    computeLocalIdx(sym)
  end localIdx

private[semanticdb] object SemanticSymbolBuilder:
  def inverseSymbol(sym: String)(using ctx: Context): List[Symbol] =
    def stripBackticks(s: String): String = s.stripPrefix("`").stripSuffix("`")

    import Scala3.StringOps.*

    val defns = ctx.definitions
    import defns.*

    def loop(s: String): List[Symbol] =
      if !s.isSymbol || s.isRootPackage then RootPackage :: Nil
      else if s.isEmptyPackage then EmptyPackageVal :: Nil
      else if s.isPackage then
        try
          val pkg = s.split('/').map(stripBackticks).mkString(".")
          requiredPackage(pkg) :: Nil
        catch
          case NonFatal(_) =>
            Nil
      else
        val (desc, parent) = DescriptorParser(s)
        val parentSymbol = loop(parent)

        def tryMember(sym: Symbol): List[Symbol] =
          sym match
            case NoSymbol =>
              Nil
            case owner =>
              desc match
                case Descriptor.None =>
                  Nil
                case Descriptor.Type(value) =>
                  val typeSym = owner.info.decl(typeName(value)).symbol
                  // Semanticdb describes java static members as a reference from type
                  //   while scalac puts static members into synthetic companion class - term
                  // To avoid issues with resolving static members return type and term in case of Java type
                  // Example:
                  //   `java/nio/file/Files#exists()` - `exists` is a member of type `Files#`
                  //   however in scalac this method is defined only in `module Files`
                  if typeSym.is(JavaDefined) then
                    typeSym :: owner.info.decl(termName(value)).symbol :: Nil
                  /**
                   * Looks like decl doesn't work for:
                   *  package a:
                   *   implicit class <<A>> (i: Int):
                   *      def inc = i + 1
                   */
                  else if typeSym == NoSymbol then
                    owner.info.member(typeName(value)).symbol :: Nil
                  else typeSym :: Nil
                  end if
                case Descriptor.Term(value) =>
                  val outSymbol = owner.info.decl(termName(value)).symbol
                  if outSymbol.exists
                  then outSymbol :: Nil
                  else if owner.is(Package)
                  then
                    // Fallback for type companion objects,
                    // e.g.
                    // ```File.scala
                    // package a
                    // type Cow = Int
                    // object Cow
                    // ```
                    // `ScalaTopLevelMtags` emits `a/Cow.`
                    // but the symbol we look for is `a/File$package/Cow.`
                    // (look: tests.pc.CompletionWorkspaceSuite.type-apply)
                    owner.info.decls
                      .filter { s =>
                        s.isPackageObject && s.name.stripModuleClassSuffix.show.endsWith("$package")
                      }
                      .flatMap(tryMember)
                  else Nil
                  end if
                case Descriptor.Package(value) =>
                  owner.info.decl(termName(value)).symbol :: Nil
                case Descriptor.Parameter(value) =>
                  // TODO - need to check how to implement this properly
                  // owner.paramSymss.flatten.filter(_.name.containsName(value))
                  Nil
                case Descriptor.TypeParameter(value) =>
                  // TODO - need to check how to implement this properly
                  // owner.typeParams.filter(_.name.containsName(value))
                  Nil
                case Descriptor.Method(value, _) =>
                  owner.info
                    .decl(termName(value))
                    .alternatives
                    .iterator
                    .map(_.symbol)
                    .filter(sym => symbolName(sym) == s)
                    .toList
          end match
        end tryMember

        parentSymbol.flatMap(tryMember)
    try
      val res = loop(sym)
      res.filterNot(_ == NoSymbol)
    catch case NonFatal(e) => Nil
  end inverseSymbol

  private def addName(b: StringBuilder, name: Name): Unit =
    val str = name.toString.unescapeUnicode
    if str.isJavaIdent then b.append(str)
    else b.append('`').append(str).append('`')

  /** Add semanticdb name of the given symbol to string builder */
  private def addSymName(b: StringBuilder, sym: Symbol, sb: Option[SemanticSymbolBuilder])(using Context): Unit =

    def addOwner(owner: Symbol): Unit =
      if !owner.isRoot then
        // Skip synthetic refinement class so refinement members get the type alias as owner
        // e.g. User#name(). instead of User#`<refinement>`#name().
        if owner.name == tpnme.REFINE_CLASS then addOwner(owner.owner)
        else addSymName(b, owner, sb)

    def addOverloadIdx(initSym: Symbol): Unit =
      // revert from the compiler-generated overload of the signature polymorphic method
      val sym = initSym.originalSignaturePolymorphic.symbol.orElse(initSym)
      val decls =
        val decls0 = sym.owner.info.decls.lookupAll(sym.name)
        if sym.owner.isAllOf(JavaModule) then
          decls0 ++ sym.owner.companionClass.info.decls.lookupAll(sym.name)
        else
          decls0
      end decls
      val alts = decls.filter(_.isOneOf(Method | Mutable)).toList.reverse.partition(!_.is(Synthetic)).toList.flatten

      def find(filter: Symbol => Boolean) = alts match
        case notSym :: rest if !filter(notSym) =>
          val idx = rest.indexWhere(filter)
          if idx >= 0 then b.append('+').append(idx + 1)
        case _ =>
      end find

      try
        val sig = sym.signature
        val targetName = sym.targetName
        find(sym => sym.signature == sig && sym.targetName == targetName)
      catch
        // sym.signature might not exist
        // this solves tests/best-effort/compiler-semanticdb-crash
        case _: MissingType if ctx.usedBestEffortTasty =>

    @tailrec
    def addDescriptor(sym: Symbol): Unit =
      if sym.is(ModuleClass) then
        addDescriptor(sym.sourceModule)
      else if sym.is(TypeParam) then
        b.append('[')
        addName(b, sym.name)
        b.append(']')
      else if sym.is(Param) then
        b.append('(')
        addName(b, sym.name)
        b.append(')')
      else if sym.isRoot then
        b.append(Symbols.RootPackage)
      else if sym.isEmptyPackage then
        b.append(Symbols.EmptyPackage)
      else if sym.isScala2PackageObject then
        b.append(Symbols.PackageObjectDescriptor)
      else
        addName(b, sym.name)
        if sym.is(Package) then b.append('/')
        else if sym.isType || sym.isAllOf(JavaModule) then b.append('#')
        else if sym.is(Method) || (sym.is(Mutable) && !sym.is(JavaDefined))
          && (!sym.is(StableRealizable) || sym.isConstructor) then
          b.append('(')
          addOverloadIdx(sym)
          b.append(").")
        else b.append('.')

    sb match
      case Some(sb) if sym.exists && !sym.isGlobal =>
        b.append(Symbols.LocalPrefix).append(sb.localIdx(sym))
      case _ if sym.exists =>
        addOwner(sym.owner)
        addDescriptor(sym)
      case _ =>
        ()

  end addSymName

  def symbolName(sym: Symbol)(using Context): String =
    val b = StringBuilder(20)
    addSymName(b, sym, None)
    b.toString
