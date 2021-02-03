package dotty.tools
package backend
package jvm

import dotc.CompilationUnit
import dotc.ast.tpd._
import dotc.util.{ SourcePosition, SourceFile }
import dotc.core.Contexts._
import dotc.typer.Inliner.InliningPosition
import collection.mutable

import scala.collection.mutable.StringBuilder

class InlinedsPositioner(cunit: CompilationUnit)(using Context):
  private case class Request(targetPos: SourcePosition, origPos: SourcePosition, firstFakeLine: Int)

  private class File(id: Int, name: String, path: Option[String]):
    def write(b: mutable.StringBuilder): Unit =
      if path.isDefined then b ++= "+ "
      b append id
      b += ' '
      b ++= name
      b += '\n'
      path.foreach { p =>
        b ++= p
        b += '\n'
      }
  end File

  private class Mapping(
    inputStartLine: Int,
    fileId: Int,
    repeatCount: Int,
    outputStartLine: Int,
    increment: Int
  ):
    extension (b: mutable.StringBuilder) def appendNotDefault(prefix: Char, value: Int): Unit =
      if value != 1 then
        b += prefix
        b append value

    def write(b: mutable.StringBuilder): Unit =
      b append (inputStartLine + 1)
      b.appendNotDefault('#', fileId)
      b.appendNotDefault(',', repeatCount)
      b += ':'
      b append (outputStartLine + 1)
      b.appendNotDefault(',', increment)
      b += '\n'
  end Mapping

  private class Stratum(name: String, files: List[File], mappings: List[Mapping]):
      def write(b: mutable.StringBuilder): Unit =
        b ++= "*S "
        b ++= name
        b ++= "\n*F\n"
        files.foreach(_.write(b))
        b ++= "*L\n"
        mappings.foreach(_.write(b))
        b ++= "*E\n"

  private val requests = mutable.ListBuffer.empty[Request]
  private val allocatedLines = mutable.Map.empty[SourcePosition, Int]
  private var lastLine = cunit.tpdTree.sourcePos.endLine

  private def allocate(origPos: SourcePosition): Int =
    val line = lastLine + 1
    lastLine += origPos.lines.length
    allocatedLines += (origPos -> line)
    line

  private class RequestCollector(enclosingFile: SourceFile) extends TreeTraverser:
    override def traverse(tree: Tree)(using Context): Unit =
      if tree.source != enclosingFile then
        tree.getAttachment(InliningPosition) match
          case Some(targetPos) =>
            val firstFakeLine = allocatedLines.applyOrElse(tree.sourcePos, allocate)
            requests += Request(targetPos, tree.sourcePos, firstFakeLine)
            RequestCollector(tree.source).traverseChildren(tree)
          case None =>
            // Not exactly sure in which cases it is happening. Should we report warning?
            RequestCollector(tree.source).traverseChildren(tree)
      else traverseChildren(tree)
  end RequestCollector

  RequestCollector(cunit.source).traverse(cunit.tpdTree)

  def debugExtension: Option[String] = Option.when(requests.nonEmpty) {
    val scalaStratum =
      val files = cunit.source :: requests.map(_.origPos.source).distinct.filter(_ != cunit.source).toList
      val mappings = requests.distinctBy(_.firstFakeLine).map { case Request(_, origPos, firstFakeLine) =>
        Mapping(origPos.startLine, files.indexOf(origPos.source) + 1, origPos.lines.length, firstFakeLine, 1)
      }.toList
      Stratum("Scala", files.zipWithIndex.map { case (f, n) => File(n + 1, f.name, None) }, Mapping(0, 1, cunit.tpdTree.sourcePos.lines.length, 0, 1) +: mappings)

    val debugStratum =
      val mappings = requests.map { case Request(targetPos, origPos, firstFakeLine) =>
        Mapping(targetPos.startLine, 1, 1, firstFakeLine, origPos.lines.length)
      }.toList
      Stratum("ScalaDebug", File(1, cunit.source.name, None) :: Nil, mappings)


    val b = new StringBuilder
    b ++= "SMAP\n"
    scalaStratum.write(b)
    debugStratum.write(b)
    b.toString
  }



