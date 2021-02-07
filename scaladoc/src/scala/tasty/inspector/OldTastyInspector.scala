package scala.tasty.inspector

import scala.quoted._
import scala.quoted.runtime.impl.QuotesImpl

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.Driver
import dotty.tools.dotc.Run
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.fromtasty._
import dotty.tools.dotc.util.ClasspathFromClassloader
import dotty.tools.dotc.CompilationUnit
import dotty.tools.unsupported
import dotty.tools.dotc.report

import java.io.File.pathSeparator

// COPY OF OLD IMPLEMENTATION
// TODO: update to new implementation
trait OldTastyInspector:
  self =>

  /** Process a TASTy file using TASTy reflect */
  protected def processCompilationUnit(using Quotes)(root: quotes.reflect.Tree): Unit

  /** Called after all compilation units are processed */
  protected def postProcess(using Quotes): Unit = ()

  /** Load and process TASTy files using TASTy reflect
   *
   *  @param tastyFiles List of paths of `.tasty` files
   */
  def inspectTastyFiles(tastyFiles: List[String]): Boolean =
    inspectAllTastyFiles(tastyFiles, Nil, Nil)

  /** Load and process TASTy files in a `jar` file using TASTy reflect
   *
   *  @param jars Path of `.jar` file
   */
  def inspectTastyFilesInJar(jar: String): Boolean =
    inspectAllTastyFiles(Nil, List(jar), Nil)

  /** Load and process TASTy files using TASTy reflect
   *
   *  @param tastyFiles List of paths of `.tasty` files
   *  @param jars List of path of `.jar` files
   *  @param dependenciesClasspath Classpath with extra dependencies needed to load class in the `.tasty` files
   */
  def inspectAllTastyFiles(tastyFiles: List[String], jars: List[String], dependenciesClasspath: List[String]): Boolean =
    def checkFile(fileName: String, ext: String): Unit =
      val file = dotty.tools.io.Path(fileName)
      if file.extension != ext then
        throw new IllegalArgumentException(s"File extension is not `.$ext`: $file")
      else if !file.exists then
        throw new IllegalArgumentException(s"File not found: ${file.toAbsolute}")
    tastyFiles.foreach(checkFile(_, "tasty"))
    jars.foreach(checkFile(_, "jar"))
    val files = tastyFiles ::: jars
    files.nonEmpty && inspectFiles(dependenciesClasspath, files)

  /** Load and process TASTy files using TASTy reflect and provided context
   *
   *  Used in doctool to reuse reporter and setup provided by sbt
   *
   *  @param classes List of paths of `.tasty` and `.jar` files (no validation is performed)
   *  @param classpath Classpath with extra dependencies needed to load class in the `.tasty` files
   */
  protected[inspector] def inspectFilesInContext(classpath: List[String], classes: List[String])(using Context): Unit =
    if (classes.isEmpty) report.error("Parameter classes should no be empty")
    inspectorDriver().process(inspectorArgs(classpath, classes), summon[Context])


  private def inspectorDriver() =
    class InspectorDriver extends Driver:
      override protected def newCompiler(implicit ctx: Context): Compiler = new TastyFromClass

    class TastyInspectorPhase extends Phase:
      override def phaseName: String = "tastyInspector"

      override def run(implicit ctx: Context): Unit =
        val qctx = QuotesImpl()
        self.processCompilationUnit(using qctx)(ctx.compilationUnit.tpdTree.asInstanceOf[qctx.reflect.Tree])

    class TastyInspectorFinishPhase extends Phase:
      override def phaseName: String = "tastyInspectorFinish"

      override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
        val qctx = QuotesImpl()
        self.postProcess(using qctx)
        units

      override def run(implicit ctx: Context): Unit = unsupported("run")

    class TastyFromClass extends TASTYCompiler:

      override protected def frontendPhases: List[List[Phase]] =
        List(new ReadTasty) :: // Load classes from tasty
        Nil

      override protected def picklerPhases: List[List[Phase]] = Nil

      override protected def transformPhases: List[List[Phase]] = Nil

      override protected def backendPhases: List[List[Phase]] =
        List(new TastyInspectorPhase) ::  // Perform a callback for each compilation unit
        List(new TastyInspectorFinishPhase) :: // Perform a final callback
        Nil

      override def newRun(implicit ctx: Context): Run =
        reset()
        new TASTYRun(this, ctx.fresh.addMode(Mode.ReadPositions).addMode(Mode.ReadComments))

    new InspectorDriver

  private def inspectorArgs(classpath: List[String], classes: List[String]): Array[String] =
    val currentClasspath = ClasspathFromClassloader(getClass.getClassLoader)
    val fullClasspath = (classpath :+ currentClasspath).mkString(pathSeparator)
    ("-from-tasty" :: "-Yretain-trees" :: "-classpath" :: fullClasspath :: classes).toArray


  private def inspectFiles(classpath: List[String], classes: List[String]): Boolean =
    if (classes.isEmpty)
      throw new IllegalArgumentException("Parameter classes should no be empty")

    val reporter = inspectorDriver().process(inspectorArgs(classpath, classes))
    reporter.hasErrors

  end inspectFiles


end OldTastyInspector
