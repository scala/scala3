package dotty.tools.dotc
package core
package tasty

import java.nio.file.{Files as JFiles, Path as JPath}
import java.nio.channels.ClosedByInterruptException
import dotty.tools.io.{Path, PlainFile}
import dotty.tools.dotc.core.Contexts.Context

object BestEffortTastyWriter:

  def write(dir: JPath, units: List[CompilationUnit])(using Context): Unit =
    if JFiles.exists(dir) then JFiles.createDirectories(dir)

    units.foreach { unit =>
      unit.pickled.foreach { (clz, binary) =>
        val parts = clz.fullName.mangledString.split('.')
        val outPath = outputPath(parts.toList, dir)
        val outTastyFile = PlainFile(Path(outPath))
        val outstream = outTastyFile.output()
        try outstream.write(binary())
        catch case ex: ClosedByInterruptException =>
          JFiles.delete(outPath) // don't leave an empty or half-written tastyfile around after an interrupt
          throw ex
        finally outstream.close()
      }
    }

  def outputPath(parts: List[String], acc: JPath): JPath =
    parts match
      case Nil => throw new Exception("Invalid class name")
      case last :: Nil =>
        val name = last.stripSuffix("$")
        acc.resolve(s"$name.betasty")
      case pkg :: tail =>
        val next = acc.resolve(pkg)
        if !JFiles.exists(next) then JFiles.createDirectory(next)
        outputPath(tail, next)
