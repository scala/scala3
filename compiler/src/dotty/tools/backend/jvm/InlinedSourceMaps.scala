package dotty.tools
package backend
package jvm

import dotc.CompilationUnit
import dotc.ast.tpd._
import dotc.util.{ SourcePosition, SourceFile }
import dotc.core.Contexts._
import dotc.core.Symbols.Symbol
import dotc.report
import dotc.typer.Inliner.InliningPosition
import collection.mutable


object InlinedSourceMaps:
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
  end Stratum

  def sourceMapFor(cunit: CompilationUnit)(internalNameProvider: Symbol => String)(using Context): InlinedSourceMap =
    val requests = mutable.ListBuffer.empty[Request]
    var lastLine = cunit.tpdTree.sourcePos.endLine
    var internalNames = Map.empty[SourceFile, String]

    class RequestCollector(enclosingFile: SourceFile) extends TreeTraverser:
      override def traverse(tree: Tree)(using Context): Unit =
        if tree.source != enclosingFile && tree.source != cunit.source then
          tree.getAttachment(InliningPosition) match
            case Some(InliningPosition(targetPos, cls)) =>
              val firstFakeLine = allocate(tree.sourcePos)
              requests += Request(targetPos, tree.sourcePos, firstFakeLine)
              cls match
                case Some(symbol) if !internalNames.isDefinedAt(tree.source) =>
                  internalNames += (tree.source -> internalNameProvider(symbol))
                  // We are skipping any internal name info if we already have one stored in our map
                  // because a debugger will use internal name only to localize matching source.
                  // Both old and new internal names are associated with the same source file
                  // so it doesn't matter if internal name is not matching used symbol.
                case _ => ()
              RequestCollector(tree.source).traverseChildren(tree)
            case None =>
              // Not exactly sure in which cases it is happening. Should we report warning?
              RequestCollector(tree.source).traverseChildren(tree)
        else traverseChildren(tree)
    end RequestCollector

    def allocate(origPos: SourcePosition): Int =
      val line = lastLine + 1
      lastLine += origPos.lines.length
      line

    RequestCollector(cunit.source).traverse(cunit.tpdTree)
    InlinedSourceMap(cunit, requests.toList, internalNames)
  end sourceMapFor

  class InlinedSourceMap private[InlinedSourceMaps] (
    cunit: CompilationUnit,
    requests: List[Request],
    internalNames: Map[SourceFile, String])(using Context):

    def debugExtension: Option[String] = Option.when(requests.nonEmpty) {
      val scalaStratum =
        val files = cunit.source :: requests.map(_.origPos.source).distinct.filter(_ != cunit.source)
        val mappings = requests.map { case Request(_, origPos, firstFakeLine) =>
          Mapping(origPos.startLine, files.indexOf(origPos.source) + 1, origPos.lines.length, firstFakeLine, 1)
        }
        Stratum("Scala",
          files.zipWithIndex.map { case (f, n) => File(n + 1, f.name, internalNames.get(f)) },
          Mapping(0, 1, cunit.tpdTree.sourcePos.lines.length, 0, 1) +: mappings
        )

      val debugStratum =
        val mappings = requests.map { case Request(targetPos, origPos, firstFakeLine) =>
          Mapping(targetPos.startLine, 1, 1, firstFakeLine, origPos.lines.length)
        }
        Stratum("ScalaDebug", File(1, cunit.source.name, None) :: Nil, mappings)


      val b = new StringBuilder
      b ++= "SMAP\n"
      b ++= cunit.source.name
      b += '\n'
      b ++= "Scala\n"
      scalaStratum.write(b)
      debugStratum.write(b)
      b.toString
    }

    def lineFor(sourcePos: SourcePosition): Option[Int] =
      requests.find(_.origPos.contains(sourcePos)) match
        case Some(request) =>
          val offset = sourcePos.startLine - request.origPos.startLine
          Some(request.firstFakeLine + offset + 1)
        case None =>
          report.warning(s"${sourcePos.show} was inlined in ${cunit.source} but its inlining position was not recorded.")
          None


