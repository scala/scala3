package scala.tasty.inspector

import scala.tasty.Reflection

import dotty.tools.dotc.Compiler
import dotty.tools.dotc.Driver
import dotty.tools.dotc.Run
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Mode
import dotty.tools.dotc.core.Phases.Phase
import dotty.tools.dotc.fromtasty._
import dotty.tools.dotc.quoted.reflect.ReflectionImpl
import dotty.tools.dotc.util.ClasspathFromClassloader

import java.io.File.pathSeparator

trait TastyInspector:
  self =>

  /** Process a TASTy file using TASTy reflect */
  protected def processCompilationUnit(reflect: Reflection)(root: reflect.Tree): Unit

  /** Load and process TASTy files using TASTy reflect
   *
   *  @param classpath Classpath where the classes are located
   *  @param classes classes to be inspected
   */
  def inspect(classpath: String, classes: List[String]): Unit =
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
        val reflect = ReflectionImpl(ctx)
        self.processCompilationUnit(reflect)(ctx.compilationUnit.tpdTree.asInstanceOf[reflect.Tree])

    end TastyInspectorPhase

    val currentClasspath = ClasspathFromClassloader(getClass.getClassLoader)
    val args = "-from-tasty" :: "-Yretain-trees" :: "-classpath" :: s"$classpath$pathSeparator$currentClasspath" :: classes
    (new InspectorDriver).process(args.toArray)
  end inspect


end TastyInspector
