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
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.transform.Pickler

/** Compiler for TASTY files.
 *  Usage:
 *
 *    scala dotty.tools.dotc.FromTasty (option | classname)*
 *
 *  Options are as for dotc.
 *  Classnames are fully qualified names of top-level classes that need to have a TASTY attribute.
 *  Example:
 *
 *    scala dotty.tools.dotc.FromTasty -Xprint:front extMethods.T
 */
object FromTasty extends Driver {
  override def newCompiler(implicit ctx: Context): Compiler = new TASTYCompiler

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
      new TASTYRun(this, ctx)
    }
  }

  class TASTYRun(comp: Compiler, ictx: Context) extends Run(comp, ictx) {
    override def compile(classNames: List[String]) = {
      units = classNames.map(new TASTYCompilationUnit(_))
      compileUnits()
    }
  }

  class TASTYCompilationUnit(val className: String) extends CompilationUnit(NoSource) {
    override def toString = s"class file $className"
  }

  class ReadTastyTreesFromClasses extends FrontEnd {

    override def isTyper = false

    override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] =
      units.flatMap(readTASTY)

    def readTASTY(unit: CompilationUnit)(implicit ctx: Context): List[CompilationUnit] = unit match {
      case unit: TASTYCompilationUnit =>
        assert(ctx.settings.YretainTrees.value)
        val className = unit.className.toTypeName
        val compilationUnits = List(tree(className), tree(className.moduleClassName)).flatMap {
          case Some((clsd, unpickled)) if !unpickled.isEmpty =>
            List(CompilationUnit.mkCompilationUnit(clsd, unpickled, forceTrees = true))
          case _ => Nil
        }
        compilationUnits
    }

    private def tree(className: TypeName)(implicit ctx: Context): Option[(ClassDenotation, tpd.Tree)] = {
      val clsd = ctx.base.staticRef(className)
      ctx.base.staticRef(className) match {
        case clsd: ClassDenotation =>
          def cannotUnpickle(reason: String) =
            ctx.error(s"class $className cannot be unpickled because $reason")
          def tryToLoad = clsd.infoOrCompleter match {
            case info: ClassfileLoader =>
              info.load(clsd)
              Option(clsd.symbol.asClass.tree).orElse {
                cannotUnpickle(s"its class file ${info.classfile} does not have a TASTY attribute")
                None
              }

            case info =>
              cannotUnpickle(s"its info of type ${info.getClass} is not a ClassfileLoader")
              None
          }
          Option(clsd.symbol.asClass.tree).orElse(tryToLoad).map(tree => (clsd, tree))

        case _ =>
          ctx.error(s"class not found: $className")
          None
      }
    }
  }
}
