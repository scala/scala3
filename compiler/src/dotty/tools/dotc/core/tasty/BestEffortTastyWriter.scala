package dotty.tools.dotc
package core
package tasty

import dotty.tools.nio.*
import dotty.tools.dotc.core.Contexts.Context

object BestEffortTastyWriter:

  def write(dir: FileContainer, units: List[CompilationUnit])(using Context): Unit =
    units.foreach { unit =>
      unit.pickled.foreach { (clz, binary) =>
        val parts = clz.fullName.mangledString.split('.')
        val outTastyFile = outputPath(parts.toList, dir)
        outTastyFile.writeBytes(binary())
      }
    }

  def outputPath(parts: List[String], acc: FileContainer): File =
    parts match
      case Nil => throw new Exception("Invalid class name")
      case last :: Nil =>
        val name = last.stripSuffix("$")
        acc.getOrCreateFile(s"$name.betasty")
      case pkg :: tail =>
        val next = acc.getOrCreateContainer(pkg)
        outputPath(tail, next)
