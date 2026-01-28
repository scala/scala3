package dotty.tools.pc.utils

import java.io.File
import java.nio.file.Paths

import scala.collection.mutable
import scala.language.unsafeNulls
import scala.meta.internal.metals.CompilerVirtualFileParams
import scala.meta.internal.metals.Fuzzy
import scala.meta.internal.metals.WorkspaceSymbolQuery
import scala.meta.pc.SymbolSearchVisitor

import dotty.tools.dotc.ast.untpd.*
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Flags
import dotty.tools.dotc.interactive.InteractiveDriver
import dotty.tools.pc.CompilerSearchVisitor
import dotty.tools.pc.utils.InteractiveEnrichments.decoded

import TestingWorkspaceSearch.*

object TestingWorkspaceSearch:
  def empty: TestingWorkspaceSearch = new TestingWorkspaceSearch(Nil)
  class Disambiguator:
    val nameMap = mutable.Map[String, Int]()
    def methodPart(name: String) =
      val i = nameMap.getOrElse(name, 0)
      nameMap.put(name, i + 1)
      if i == 0 then "()."
      else s"(+$i)."

  case class ParentSymbol(symbol: SearchSymbol, fileName: String):
    private val dis: Disambiguator = new Disambiguator
    private def isPackage = symbol.lastOption.exists(_.suffix == "/")
    private def isMethod = symbol.lastOption.exists(_.suffix.endsWith(")."))
    private def isInit = symbol.lastOption.exists(_.name == "<init>")
    private def filePackage = SymbolPart(fileName, "$package.")
    private def member(part: SymbolPart) =
      if isPackage then Some(symbol :+ filePackage :+ part)
      else if isMethod then
        if isInit then Some(symbol.dropRight(1) :+ part)
        else None
      else Some(symbol :+ part)
    def makeMethod(newPart: String) = member(SymbolPart(newPart, dis.methodPart(newPart)))
    def makeVal(newPart: String) =
      member(SymbolPart(newPart, "."))
    def makeTypeAlias(newPart: String) = member(SymbolPart(newPart, "#"))
    def makeType(newPart: String) = symbol :+ SymbolPart(newPart, "#")
    def makeTerm(newPart: String) = symbol :+ SymbolPart(newPart, ".")
    def makePackage(parts: List[String], isPackageObject: Boolean = false) =
      val suffix = if isPackageObject then "/package." else "/"
      parts match
        case "<empty>" :: Nil => List(SymbolPart("_empty_", suffix))
        case list if symbol.map(_.name) == List("_empty_") => list.map(SymbolPart(_, suffix))
        case list => symbol ++ list.map(SymbolPart(_, suffix))

  object ParentSymbol:
    def empty(fileName: String) = ParentSymbol(Nil, fileName)

  case class SymbolPart(name: String, suffix: String)
  type SearchSymbol = List[SymbolPart]

class TestingWorkspaceSearch(classpath: Seq[String]):
  val inputs: mutable.Map[String, String] = mutable.Map.empty[String, String]

  val defaultFlags = List("-color:never")

  val settings =
    defaultFlags ++
      List("-classpath", classpath.mkString(File.pathSeparator))

  private class SymbolCollector extends UntypedTreeAccumulator[List[Tree]]:
    override def apply(x: List[Tree], tree: Tree)(using Context): List[Tree] = tree :: x

    private def newSymbol(tree: Tree, parent: ParentSymbol)(using Context): Option[SearchSymbol] =
      tree match
        case PackageDef(name, _) =>
          Some(parent.makePackage(namesFromSelect(name).reverse))
        case m @ ModuleDef(name, _) if m.mods.is(Flags.Package) =>
          Some(parent.makePackage(List(name.decoded), isPackageObject = true))
        case ModuleDef(name, _) =>
          Some(parent.makeTerm(name.decoded))
        case ValDef(name, _, _) =>
          parent.makeVal(name.decoded)
        case t @ TypeDef(name, _: Template) if !t.mods.is(Flags.Implicit) =>
          Some(parent.makeType(name.decoded))
        case TypeDef(name, _) =>
          parent.makeTypeAlias(name.decoded)
        case DefDef(name, _, _, _) =>
          parent.makeMethod(name.decoded)
        case _ => None

    def traverse(acc: List[SearchSymbol], tree: Tree, parent: ParentSymbol)(using Context): List[SearchSymbol] =
      val symbol = newSymbol(tree, parent)
      val res = symbol.filter(_.lastOption.exists(_.suffix != "/")).map(_ :: acc).getOrElse(acc)
      val children = foldOver(Nil, tree).reverse
      val newParent = symbol.map(ParentSymbol(_, parent.fileName)).getOrElse(parent)
      children.foldLeft(res)((a, c) => traverse(a, c, newParent))

  val driver = new InteractiveDriver(settings)

  private def namesFromSelect(select: Tree)(using Context): List[String] =
    select match
      case Select(qual, name) => name.decoded :: namesFromSelect(qual)
      case Ident(name) => List(name.decoded)

  def search(
      query: WorkspaceSymbolQuery,
      visitor: SymbolSearchVisitor,
      filter: Symbol => Context ?=> Boolean = _ => true
  ): Unit =
    given Context = driver.currentCtx.fresh

    visitor match
      case visitor: CompilerSearchVisitor =>
        inputs.map: (path, text) =>
          val nio = Paths.get(path)
          val uri = nio.toUri()
          driver.run(uri, text)
          val run = driver.currentCtx.run
          val unit = run.units.head
          val symbols = SymbolCollector().traverse(
            Nil,
            unit.untpdTree,
            ParentSymbol.empty(nio.getFileName().toString().stripSuffix(".scala"))
          )
          symbols.foreach: sym =>
            val name = sym.last.name
            if Fuzzy.matches(query.query, name)
            then
              val symbolsString = sym.map { case SymbolPart(name, suffix) => name ++ suffix }.mkString
              visitor.visitWorkspaceSymbol(Paths.get(""), symbolsString, null, null)
      case _ =>
