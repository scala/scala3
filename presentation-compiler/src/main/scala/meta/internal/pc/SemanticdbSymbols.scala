package scala.meta.internal.pc

import scala.util.control.NonFatal

import dotty.tools.dotc.core.Contexts.*
import dotty.tools.dotc.core.Flags.*
import dotty.tools.dotc.core.Names.*
import dotty.tools.dotc.core.Symbols.*

object SemanticdbSymbols:

  def inverseSemanticdbSymbol(sym: String)(using ctx: Context): List[Symbol] =
    import scala.meta.internal.semanticdb.Scala.*

    val defns = ctx.definitions
    import defns.*

    def loop(s: String): List[Symbol] =
      if s.isNone || s.isRootPackage then RootPackage :: Nil
      else if s.isEmptyPackage then EmptyPackageVal :: Nil
      else if s.isPackage then
        try requiredPackage(s.stripSuffix("/").replace("/", ".")) :: Nil
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
                  else typeSym :: Nil
                case Descriptor.Term(value) =>
                  owner.info.decl(termName(value)).symbol :: Nil
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

        parentSymbol.flatMap(tryMember)
    try loop(sym).filterNot(_ == NoSymbol)
    catch case NonFatal(e) => Nil
  end inverseSemanticdbSymbol

  /** The semanticdb name of the given symbol */
  def symbolName(sym: Symbol)(using Context): String =
    val b = StringBuilder(20)
    addSymName(b, sym)
    b.toString

  /**
   *  Taken from https://github.com/lampepfl/dotty/blob/2db43dae1480825227eb30d291b0dd0f0494e0f6/compiler/src/dotty/tools/dotc/semanticdb/ExtractSemanticDB.scala#L293
   *  In future might be replaced by usage of compiler implementation after merging https://github.com/lampepfl/dotty/pull/12885
   */
  private def addSymName(b: StringBuilder, sym: Symbol)(using Context): Unit =

    import dotty.tools.dotc.semanticdb.Scala3.{*, given}

    def addName(name: Name) =
      val str = name.toString.unescapeUnicode
      if str.nonEmpty && str.isJavaIdent then b append str
      else b append '`' append str append '`'

    def addOwner(owner: Symbol): Unit =
      if !owner.isRoot then addSymName(b, owner)

    def addOverloadIdx(sym: Symbol): Unit =
      val decls =
        val decls0 = sym.owner.info.decls.lookupAll(sym.name)
        if sym.owner.isAllOf(JavaModule) then
          decls0 ++ sym.owner.companionClass.info.decls.lookupAll(sym.name)
        else decls0
      end decls
      // private java constructors do not have a symbol created
      val alts = decls
        .filter(d => d.isOneOf(Method | Mutable) && !d.is(Private))
        .toList
        .reverse
      def find(filter: Symbol => Boolean) = alts match
        case notSym :: rest if !filter(notSym) =>
          val idx = rest.indexWhere(filter)
          if idx >= 0 then b.append('+').append(idx + 1)
        case _ =>
      end find
      val sig = sym.signature
      find(_.signature == sig)
    end addOverloadIdx

    def addDescriptor(sym: Symbol): Unit =
      if sym.is(ModuleClass) then addDescriptor(sym.sourceModule)
      else if sym.is(TypeParam) then
        b.append('['); addName(sym.name); b.append(']')
      else if sym.is(Param) then
        b.append('('); addName(sym.name); b.append(')')
      else if sym.isRoot then b.append(Symbols.RootPackage)
      else if sym.isEmptyPackage then b.append(Symbols.EmptyPackage)
      else if sym.isScala2PackageObject then
        b.append(Symbols.PackageObjectDescriptor)
      else
        addName(sym.name)
        if sym.is(Package) then b.append('/')
        else if sym.isType || sym.isAllOf(JavaModule) then b.append('#')
        else if sym.isOneOf(Method | Mutable)
          && (!sym.is(StableRealizable) || sym.isConstructor)
        then
          b.append('('); addOverloadIdx(sym); b.append(").")
        else b.append('.')

    if !sym.isRoot && sym != NoSymbol then addOwner(sym.owner)
    addDescriptor(sym)
  end addSymName

end SemanticdbSymbols
