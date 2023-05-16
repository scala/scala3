package dotty.tools
package backend
package jvm

import dotc.CompilationUnit
import dotc.ast.tpd._
import dotc.util.{ SourcePosition, SourceFile }
import dotc.core.Contexts._
import dotc.core.Symbols.Symbol
import dotc.report
import dotc.inlines.Inlines.InliningPosition
import collection.mutable

/**
 * Tool for generating virtual lines for inlined calls and keeping track of them.

 * How it works:
 * - For every inlined call it assumes that empty lines are appended to the source file. These
 *   lines are not added anywhere in physical form. We only assume that they exist only to be used
 *   by `LineNumberTable` and `SourceDebugExtension`. The number of these virtual lines is every
 *   time equal to the size of line range of the expansion of inlined call.
 * - It generates SMAP (as defined by JSR-45) containing two strata. The first stratum (`Scala`)
 *   is describing the mapping from the real source files to the real and virtual lines in our
 *   assumed source. The second stratum (`ScalaDebug`) is mapping from virtual lines to
 *   corresponding inlined calls.
 * - Generated SMAP is written to the bytecode in `SourceDebugExtension`
 * - During the generation of the bytecode backed is asking `InlinedSourceMap` about position of
 *   all trees that have source different from the main source of given compilation unit.
 *   The response to that request is number of the virtual line that is corresponding to particular
 *   line from the other source.
 * - Debuggers can use information stored in `LineNumberTable` and `SourceDebugExtension` to
 *   correctly guess which line of inlined method is currently executed. They can also construct
 *   stack frames for inlined calls.
 **/
object InlinedSourceMaps:
  //private case class Request(targetPos: SourcePosition, origPos: SourcePosition, firstFakeLine: Int)
  private case class Request(inline: Inlined, firstFakeLine: Int)

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
    val requests = mutable.ListBuffer.empty[Inlined]
    var internalNames = Map.empty[SourceFile, String]

    class RequestCollector(enclosingFile: SourceFile) extends TreeTraverser:
      override def traverse(tree: Tree)(using Context): Unit =
        tree match
          case Inlined(call, bindings, expansion) =>
            if expansion.source != enclosingFile && expansion.source != cunit.source then
              requests += tree.asInstanceOf[Inlined]
              val topLevelClass = Option.when(!call.isEmpty)(call.symbol.topLevelClass)

              topLevelClass match
                case Some(symbol) if !internalNames.isDefinedAt(tree.source) =>
                  internalNames += (tree.source -> internalNameProvider(symbol))
                  // We are skipping any internal name info if we already have one stored in our map
                  // because a debugger will use internal name only to localize matching source.
                  // Both old and new internal names are associated with the same source file
                  // so it doesn't matter if internal name is not matching used symbol.
                case _ => ()

            traverseChildren(tree)
          case _ => traverseChildren(tree)
    end RequestCollector

    // Don't generate mappings for the quotes compiled at runtime by the staging compiler
    if cunit.source.file.isVirtual then InlinedSourceMap(cunit, Nil, Map.empty[SourceFile, String])
    else
      var lastLine = cunit.tpdTree.sourcePos.endLine
      // returns the first fake line (starting from 0) 
      def allocate(origPos: SourcePosition): Int =
        val line = lastLine + 1
        lastLine += origPos.lines.length
        line

      RequestCollector(cunit.source).traverse(cunit.tpdTree)

      val allocated = requests.map(r => Request(r, allocate(r.expansion.sourcePos)))

      InlinedSourceMap(cunit, allocated.toList, internalNames)
  end sourceMapFor

  class InlinedSourceMap private[InlinedSourceMaps] (
    cunit: CompilationUnit,
    requests: List[Request],
    internalNames: Map[SourceFile, String])(using Context):

    def debugExtension: Option[String] = Some("TODO")

    private val inlines = mutable.ListBuffer.empty[Inlined]

    def lineFor(tree: Tree): Option[Int] =

      tree match
        case Inlined(call, binding, expansion) =>
          inlines += tree.asInstanceOf[Inlined]
          None
        case _ => 
          val sourcePos = tree.sourcePos
          val inline = inlines.findLast(_.expansion.contains(tree))
          requests.findLast(r => r.inline.expansion.contains(tree)) match
            case Some(request) =>
              val offset = sourcePos.startLine - request.inline.expansion.sourcePos.startLine
              val virtualLine = request.firstFakeLine + offset
              if requests.filter(_.inline.expansion.contains(tree)).size > 1 then None
              else Some(virtualLine + 1) // + 1 because the first line is 1 in the LineNumberTable
            case None =>
              // report.warning(s"${sourcePos.show} was inlined in ${cunit.source} but its inlining position was not recorded.")
              None