package dotty.tools
package dotc

import core._
import Contexts._, Periods._, Symbols._
import io.PlainFile
import util.{SourceFile, NoSource, Stats, SimpleMap}
import reporting.Reporter
import java.io.{BufferedWriter, OutputStreamWriter}
import scala.reflect.io.VirtualFile

class Run(comp: Compiler)(implicit ctx: Context) {

  var units: List[CompilationUnit] = _

  def getSource(fileName: String): SourceFile = {
    val f = new PlainFile(fileName)
    if (f.exists) new SourceFile(f)
    else {
      ctx.error(s"not found: $fileName")
      NoSource
    }
  }

  def compile(fileNames: List[String]): Unit = {
    val sources = fileNames map getSource
    compileSources(sources)
  }

  def compileSources(sources: List[SourceFile]) = Stats.monitorHeartBeat {
    if (sources forall (_.exists)) {
      units = sources map (new CompilationUnit(_))
      for (phase <- ctx.allPhases)
        phase.runOn(units)
    }
  }

  def compile(sourceCode: String): Unit = {
    val virtualFile = new VirtualFile(sourceCode) // use source code as name as it's used for equals
    val writer = new BufferedWriter(new OutputStreamWriter(virtualFile.output, "UTF-8")) // buffering is still advised by javadoc
    writer.write(sourceCode)
    writer.close()
    compileSources(List(new SourceFile(virtualFile)))
  }

  /** Print summary; return # of errors encountered */
  def printSummary(): Reporter = {
    ctx.runInfo.printMaxConstraint()
    val r = ctx.typerState.reporter
    r.printSummary
    r
  }
}