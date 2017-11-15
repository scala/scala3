package dotty.tools
package dotc
package fromtasty

import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.Decorators._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.NameOps._
import dotty.tools.dotc.core.SymDenotations.ClassDenotation
import dotty.tools.dotc.core._
import dotty.tools.dotc.typer.FrontEnd

class ReadTastyTreesFromClasses extends FrontEnd {

  override def isTyper = false

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] =
    units.flatMap(readTASTY)

  def readTASTY(unit: CompilationUnit)(implicit ctx: Context): Option[CompilationUnit] = unit match {
    case unit: TASTYCompilationUnit =>
      val className = unit.className.toTypeName
      def compilationUnit(className: TypeName): Option[CompilationUnit] = {
        tree(className).flatMap {
          case (clsd, unpickled) =>
            if (unpickled.isEmpty) None
            else Some(CompilationUnit.mkCompilationUnit(clsd, unpickled, forceTrees = true))

        }
      }
      // The TASTY section in a/b/C.class may either contain a class a.b.C, an object a.b.C, or both.
      // We first try to load the class and fallback to loading the object if the class doesn't exist.
      // Note that if both the class and the object are present, then loading the class will also load
      // the object, this is why we use orElse here, otherwise we could load the object twice and
      // create ambiguities!
      compilationUnit(className).orElse(compilationUnit(className.moduleClassName))
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
