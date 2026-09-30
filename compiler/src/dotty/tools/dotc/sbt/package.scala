package dotty.tools.dotc.sbt

import dotty.tools.dotc.core.Contexts.{Context, ctx}
import dotty.tools.dotc.core.Symbols.Symbol
import dotty.tools.dotc.core.NameOps.stripModuleClassSuffix
import dotty.tools.dotc.core.Names.Name
import dotty.tools.dotc.core.Names.termName

import interfaces.IncrementalCallback
import dotty.tools.dotc.transform.Pickler.BufferingReporter
import dotty.tools.dotc.core.Decorators.em
import dotty.tools.io.{File, FileExtension}

import java.io.PrintWriter
import scala.io.Codec

inline val TermNameHash = 1987 // 300th prime
inline val TypeNameHash = 1993 // 301st prime
inline val InlineParamHash = 1997 // 302nd prime

/** Write to the `.inc` file next to the current unit's source, for `-Ydump-sbt-inc`.
 *  The file is started fresh by `ExtractDependencies`, the API and the dependencies are appended.
 */
def writeIncFile(append: Boolean)(op: PrintWriter => Unit)(using Context): Unit =
  ctx.compilationUnit.source.jfile.ifPresent: jpath =>
    val file = File(jpath.toPath)(using Codec.UTF8).changeExtension(FileExtension.Inc).toFile
    val pw = PrintWriter(file.bufferedWriter(append), true)
    try op(pw) finally pw.close()

def asyncZincPhaseCompleted(pending: Option[BufferingReporter], phase: String)(signal: => Unit): BufferingReporter =
  val zincReporter = pending match
    case Some(buffered) => buffered
    case None => BufferingReporter()
  try signal
  catch
    case t: Exception =>
      zincReporter.exception(em"signaling $phase phase completion", t)
  zincReporter

extension (sym: Symbol)

  /** Mangle a JVM symbol name in a format better suited for internal uses by sbt.
   *  WARNING: output must not be written to TASTy, as it is not a valid TASTy name.
   */
  private[sbt] def zincMangledName(using Context): Name =
    if sym.isConstructor then
      // TODO: ideally we should avoid unnecessarily caching these Zinc specific
      // names in the global chars array. But we would need to restructure
      // ExtractDependencies caches to avoid expensive `toString` on
      // each member reference.
      termName(sym.owner.fullName.mangledString.replace(".", ";") + ";init;")
    else
      sym.name.stripModuleClassSuffix
