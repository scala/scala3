/* dotc
 * Copyright 2005-2015 LAMP/EPFL
 * @author  Martin Odersky
 */
package dotty.tools
package dotc

import core._
import Contexts._
import Symbols._
import SymDenotations._
import typer.FrontEnd
import Phases.Phase
import util._
import Decorators._
import dotty.tools.dotc.transform.Pickler
import pickling.DottyUnpickler

object FromTasty extends Driver {
  override def newCompiler(): Compiler = new TASTYCompiler

  class TASTYCompiler extends Compiler {

    override def phases: List[List[Phase]] = {
      val backendPhases = super.phases.dropWhile {
        case List(_: Pickler) => false
        case _ => true
      }.tail
      List(new ReadTastyTreesFromClasses) :: backendPhases
    }

    override def newRun(implicit ctx: Context): Run = {
      reset()
      new TASTYRun(this)(rootContext)
    }
  }

  class TASTYRun(comp: Compiler)(implicit ctx: Context) extends Run(comp) {
    override def compile(classNames: List[String]) = {
      units = classNames.map(new TASTYCompilationUnit(_))
      compileUnits()
    }
  }

  class TASTYCompilationUnit(val className: String) extends CompilationUnit(NoSource) {
    override def toString = s"class file $className"
  }

  class ReadTastyTreesFromClasses extends FrontEnd {
    override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] =
      units.map(readTASTY)

    def readTASTY(unit: CompilationUnit)(implicit ctx: Context): CompilationUnit = unit match {
      case unit: TASTYCompilationUnit =>
        val className = unit.className.toTypeName
        val clsd =
          if (className.contains('.')) ctx.base.staticRef(className)
          else defn.EmptyPackageClass.info.decl(className)
        def cannotUnpickle(reason: String) = {
          ctx.error(s"class $className cannot be unpickled because $reason")
          unit
        }
        clsd match {
          case clsd: ClassDenotation =>
            clsd.infoOrCompleter match {
              case info: ClassfileLoader =>
                info.load(clsd) match {
                  case Some(unpickler: DottyUnpickler) =>
                    val (List(unpickled), sources) = unpickler.body(readPositions = false)
                    val unit1 = sources.get(unpickled) match {
                      case Some(source) => new CompilationUnit(source)
                      case _ => unit
                    }
                    unit1.tpdTree = unpickled
                    unit1.embeddedSources = sources - unpickled
                    unit1
                  case _ =>
                    cannotUnpickle(s"it does not have a TASTY attribute")
                }
              case info =>
                cannotUnpickle(s"its info of type ${info.getClass} is not a ClassfileLoader")
            }
          case _ =>
            println(defn.EmptyPackageClass.info.decls)
            ctx.error(s"class not found: $className")
            unit
        }
    }
  }
}
