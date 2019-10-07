package dotty.tools.dotc
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
import collection.mutable
import java.lang.Character.{isJavaIdentifierPart, isJavaIdentifierStart}

abstract class ExtractSemanticDB extends Phase {
  import ast.tpd._

  override val phaseName: String = ExtractSemanticDB.name

  override def isRunnable(implicit ctx: Context) =
    super.isRunnable && ctx.settings.YretainTrees.value

  // Check not needed since it does not transform trees
  override def isCheckable: Boolean = false

  override def run(implicit ctx: Context): Unit =
    Extractor().traverse(ctx.compilationUnit.tpdTree)

  class Extractor extends TreeTraverser {

    val locals = mutable.HashMap[Symbol, java.lang.Integer]()

    private var myLocalIdx: Int = -1
    private def nextLocalIdx() =
      myLocalIdx += 1
      myLocalIdx

    def symbolName(sym: Symbol)(given ctx: Context): String =

      def isJavaIdent(str: String) =
        isJavaIdentifierStart(str.head) && str.tail.forall(isJavaIdentifierPart)

      def nameToString(name: Name) =
        val str = name.toString
        if isJavaIdent(str) then str else "`" + str + "`"

      def isGlobal(sym: Symbol): Boolean =
        sym.is(Package)
        || (sym.is(Param) || sym.owner.isClass) && isGlobal(sym.owner)

      def ownerString(owner: Symbol): String =
        if owner.isRoot then "" else symbolName(owner) + "/"

      def overloadIdx(sym: Symbol): String =
        val alts = sym.owner.info.decls.lookupAll(sym.name).toList
        if alts.tail.isEmpty then ""
        else
          val idx = alts.indexOf(sym)
          assert(idx >= 0)
          idx.toString

      def descriptor(sym: Symbol): String =
        if sym.is(ModuleClass) then
          descriptor(sym.sourceModule)
        else
          val str = nameToString(sym.name)
          if sym.is(Package) then str
          else if sym.is(Module) || sym.isGetter && !sym.is(Mutable) then str + "."
          else if sym.isType then str + "#"
          else if sym.is(Method) then str + "(" + overloadIdx(sym) + ")"
          else if sym.is(TermParam) then "(" + str + ")"
          else if sym.is(TypeParam) then "[" + str + "]"
          else throw new AssertionError(i"unhandled symbol: $sym")

      def localIdx(sym: Symbol): Int =
        locals.getOrElseUpdate(sym, nextLocalIdx())

      if sym.isRoot then "_root_"
      else if sym.isEmptyPackage then "_empty_"
      else if isGlobal(sym) then ownerString(sym.owner) + descriptor(sym)
      else "local" + localIdx(sym)
    end symbolName

    def registerUse(sym: Symbol) = ???
    def registerDef(sym: Symbol) = ???

    override def traverse(tree: Tree)(given ctx: Context): Unit =
      tree match
        case tree: DefTree =>
          registerDef(tree.symbol)
          traverseChildren(tree)
        case tree: RefTree =>
          registerUse(tree.symbol)
          traverseChildren(tree)
        case tree: Import =>
          for sel <- tree.selectors do
            val imported = sel.imported.name
            if imported != nme.WILDCARD then
              for alt <- tree.expr.tpe.member(imported).alternatives do
                registerUse(alt.symbol)
        case _ =>
          traverseChildren(tree)
  }
}

object ExtractSemanticDB {
  val name: String = "extractSemanticDB"
}
