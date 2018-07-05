package dotty.tools
package dotc
package sbt

import xsbti._
import xsbti.api._

import java.io.File
import java.util.EnumSet

/** Wrapper to make an AnalysisCallback thread-safe.
 *
 *  TODO: Remove once we switch to a Zinc with
 *  https://github.com/sbt/zinc/pull/410 merged.
 */
class SynchronizedAnalysisCallback(underlying: AnalysisCallback) extends AnalysisCallback
{
  override def startSource(source: File): Unit =
    synchronized {
      underlying.startSource(source)
    }

  override def classDependency(onClassName: String, sourceClassName: String, context: DependencyContext): Unit =
    synchronized {
      underlying.classDependency(onClassName, sourceClassName, context)
    }

  override def binaryDependency(onBinaryEntry: File, onBinaryClassName: String, fromClassName: String, fromSourceFile: File, context: DependencyContext): Unit =
    synchronized {
      underlying.binaryDependency(onBinaryEntry, onBinaryClassName, fromClassName, fromSourceFile, context)
    }

  override def generatedNonLocalClass(source: File, classFile: File, binaryClassName: String, srcClassName: String): Unit =
    synchronized {
      underlying.generatedNonLocalClass(source, classFile, binaryClassName, srcClassName)
    }

  override def generatedLocalClass(source: File, classFile: File): Unit =
    synchronized {
      underlying.generatedLocalClass(source, classFile)
    }

  override def api(source: File, classApi: ClassLike): Unit =
    synchronized {
      underlying.api(source, classApi)
    }

  override def mainClass(source: File, className: String): Unit =
    synchronized {
      underlying.mainClass(source, className)
    }

  override def usedName(className: String, name: String, useScopes: EnumSet[UseScope]): Unit =
    synchronized {
      underlying.usedName(className, name, useScopes)
    }


  override def problem(what: String, pos: xsbti.Position, msg: String, severity: xsbti.Severity, reported: Boolean): Unit =
    synchronized {
      underlying.problem(what, pos, msg, severity, reported)
    }

  override def dependencyPhaseCompleted(): Unit =
    synchronized {
      underlying.dependencyPhaseCompleted()
    }

  override def apiPhaseCompleted(): Unit =
    synchronized {
      underlying.apiPhaseCompleted()
    }

  override def enabled(): Boolean =
    synchronized {
      underlying.enabled()
    }

}
