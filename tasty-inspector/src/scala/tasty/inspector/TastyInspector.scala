package scala.tasty.inspector

import scala.quoted._
import scala.quoted.internal.impl.QuoteContextImpl

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.Driver
import dotty.tools.dotc.Run
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.fromtasty._
import dotty.tools.dotc.util.ClasspathFromClassloader

import java.io.File.pathSeparator

trait TastyInspector:
  self =>

  /** Process a TASTy file using TASTy reflect */
  protected def processCompilationUnit(using QuoteContext)(root: qctx.reflect.Tree): Unit

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

  private def inspectFiles(classpath: List[String], classes: List[String]): Boolean =
    if (classes.isEmpty)
      throw new IllegalArgumentException("Parameter classes should no be empty")

    class InspectorDriver extends Driver:
      override protected def newCompiler(implicit ctx: Context): Compiler = new TastyFromClass

    class TastyFromClass extends TASTYCompiler:

      override protected def frontendPhases: List[List[Phase]] =
        List(new ReadTasty) :: // Load classes from tasty
        Nil

      override protected def picklerPhases: List[List[Phase]] = Nil

      override protected def transformPhases: List[List[Phase]] = Nil

      override protected def backendPhases: List[List[Phase]] =
        List(new TastyInspectorPhase) ::  // Print all loaded classes
        Nil

      override def newRun(implicit ctx: Context): Run =
        reset()
        new TASTYRun(this, ctx.fresh.addMode(Mode.ReadPositions).addMode(Mode.ReadComments))

    end TastyFromClass

    class TastyInspectorPhase extends Phase:

      override def phaseName: String = "tastyInspector"

      override def run(implicit ctx: Context): Unit =
        val qctx = QuoteContextImpl()
        self.processCompilationUnit(using qctx)(ctx.compilationUnit.tpdTree.asInstanceOf[qctx.reflect.Tree])

    end TastyInspectorPhase

    val currentClasspath = ClasspathFromClassloader(getClass.getClassLoader)
    val fullClasspath = (classpath :+ currentClasspath).mkString(pathSeparator)
    val args = "-from-tasty" :: "-Yretain-trees" :: "-classpath" :: fullClasspath :: classes
    val reporter = (new InspectorDriver).process(args.toArray)
    reporter.hasErrors

  end inspectFiles


end TastyInspector
