package dotty.tools
package dotc
package fromtasty

import nio.*
import core.Contexts.*
import core.Decorators.em

class TASTYRun(comp: Compiler, ictx: Context) extends Run(comp, ictx) {
  override def compile(files: List[File]): Unit = {
    val units = tastyUnits(files)
    compileUnits(units)
  }

  private def tastyUnits(files: List[File]): List[TASTYCompilationUnit] =
    val fromTastyIgnoreList = ctx.settings.YfromTastyIgnoreList.value.toSet
    // Resolve class names of tasty and jar files
    val classNames = files.flatMap { file =>
      if file.extension.isJar then
        FileContainer.getFromFile(file, ctx.settings.javaOutputVersion, ctx.settings.XjarCompressionLevel).get
          .recursiveEntries
          .filter(_.extension.isTasty)
          .map(_.path.stripPrefix("/")) // change paths from absolute to relative
          .filter(p => !fromTastyIgnoreList(p.replace("/", FileSystemEntry.separator)))
          .map(e => e.stripSuffix(".tasty").replace("/", "."))
          .toList
      else if file.extension.isTasty then TastyFileUtil.getClassName(file)
      else if file.extension.isBetasty && ctx.withBestEffortTasty then
        TastyFileUtil.getClassName(file, withBestEffortTasty = true)
      else
        report.error(em"File extension is not `tasty` or `jar`: ${file.path}")
        Nil
    }
    classNames.map(new TASTYCompilationUnit(_))
}
