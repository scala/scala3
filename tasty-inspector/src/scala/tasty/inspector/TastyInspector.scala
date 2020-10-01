package scala.tasty.inspector

import scala.quoted._

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.Driver
import dotty.tools.dotc.Run
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.fromtasty._
import dotty.tools.dotc.quoted.QuoteContextImpl
import dotty.tools.dotc.util.ClasspathFromClassloader

import java.io.File.pathSeparator

trait TastyInspector:
  self =>

  /** Process a TASTy file using TASTy reflect */
  protected def processCompilationUnit(using QuoteContext)(root: qctx.tasty.Tree): Unit

  /** Load and process TASTy files using TASTy reflect
   *
   *  @param classpath Classpath where the classes are located
   *  @param classes classes to be inspected
   *  @return if an error was reported
   */
  def inspect(classpath: String, classes: List[String]): Boolean =
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
        self.processCompilationUnit(using qctx)(ctx.compilationUnit.tpdTree.asInstanceOf[qctx.tasty.Tree])

    end TastyInspectorPhase

    val currentClasspath = ClasspathFromClassloader(getClass.getClassLoader)
    val args = "-from-tasty" :: "-Yretain-trees" :: "-classpath" :: s"$classpath$pathSeparator$currentClasspath" :: classes
    val reporter = (new InspectorDriver).process(args.toArray)
    reporter.hasErrors

  end inspect


end TastyInspector
