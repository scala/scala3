package dotty.tools
package dotc
package semanticdb

import core._
import Contexts._
import Symbols._
import Flags._
import Names.Name

import scala.annotation.tailrec
import scala.collection.mutable

class SemanticSymbolBuilder:
  import Scala3.{_, given}

  private var nextLocalIdx: Int = 0

  /** The index of a local symbol */
  private val locals = mutable.HashMap[Symbol, Int]()

  /** The local symbol(s) starting at given offset */
  private val symsAtOffset = new mutable.HashMap[Int, Set[Symbol]]():
    override def default(key: Int) = Set[Symbol]()

  def symbolName(sym: Symbol)(using Context): String =
    val b = StringBuilder(20)
    addSymName(b, sym)
    b.toString

  def funParamSymbol(sym: Symbol)(using Context): Name => String =
    if sym.isGlobal then
      val funSymbol = symbolName(sym)
      name => s"$funSymbol($name)"
    else
      name => locals.keys.find(local => local.isTerm && local.owner == sym && local.name == name)
                    .fold("<?>")(Symbols.LocalPrefix + _)

  /** Add semanticdb name of the given symbol to string builder */
  private def addSymName(b: StringBuilder, sym: Symbol)(using Context): Unit =

    def addName(name: Name) =
      val str = name.toString.unescapeUnicode
      if str.isJavaIdent then b append str
      else b append '`' append str append '`'

    def addOwner(owner: Symbol): Unit =
      if !owner.isRoot then addSymName(b, owner)

    def addOverloadIdx(sym: Symbol): Unit =
      val decls =
        val decls0 = sym.owner.info.decls.lookupAll(sym.name)
        if sym.owner.isAllOf(JavaModule) then
          decls0 ++ sym.owner.companionClass.info.decls.lookupAll(sym.name)
        else
          decls0
      end decls
      val alts = decls.filter(_.isOneOf(Method | Mutable)).toList.reverse
      def find(filter: Symbol => Boolean) = alts match
        case notSym :: rest if !filter(notSym) =>
          val idx = rest.indexWhere(filter).ensuring(_ >= 0)
          b.append('+').append(idx + 1)
        case _ =>
      end find
      val sig = sym.signature
      find(_.signature == sig)

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
    def localIdx(sym: Symbol)(using Context): Int =
      val startPos =
        assert(sym.span.exists, s"$sym should have a span")
        sym.span.start
      @tailrec
      def computeLocalIdx(sym: Symbol): Int = locals get sym match
        case Some(idx) => idx
        case None => symsAtOffset(startPos).find(_.name == sym.name) match
          case Some(other) => computeLocalIdx(other)
          case None =>
            val idx = nextLocalIdx
            nextLocalIdx += 1
            locals(sym) = idx
            symsAtOffset(startPos) += sym
            idx
      end computeLocalIdx
      computeLocalIdx(sym)
    end localIdx

    if sym.exists then
      if sym.isGlobal then
        addOwner(sym.owner); addDescriptor(sym)
      else
        b.append(Symbols.LocalPrefix).append(localIdx(sym))

  end addSymName
