package dotty.tools
package dotc
package fromtasty

import core._
import Decorators._
import Contexts.Context
import Symbols.{Symbol, ClassSymbol}
import SymDenotations.{SymDenotation, ClassDenotation, LazyType}
import typer.FrontEnd
import Names.TypeName
import NameOps._
import Types.Type
import ast.tpd
import ast.Trees.Tree
import util.SourceFile
import CompilationUnit.mkCompilationUnit

class ReadTastyTreesFromClasses extends FrontEnd {

  override def isTyper = false

  override def runOn(units: List[CompilationUnit])(implicit ctx: Context): List[CompilationUnit] =
    units.flatMap(readTASTY(_)(ctx.addMode(Mode.ReadPositions)))

  def readTASTY(unit: CompilationUnit)(implicit ctx: Context): Option[CompilationUnit] = unit match {
    case unit: TASTYCompilationUnit =>
      val className = unit.className.toTypeName

      def cannotUnpickle(reason: String): None.type = {
        ctx.error(s"class $className cannot be unpickled because $reason")
        None
      }

      def alreadyLoaded(): None.type = {
        ctx.warning(s"sclass $className cannot be unpickled because it is already loaded")
        None
      }

      def compilationUnit(cls: Symbol): Option[CompilationUnit] = cls match {
        case cls: ClassSymbol =>
          (cls.treeOrProvider: @unchecked) match {
            case unpickler: tasty.DottyUnpickler =>
              if (cls.tree.isEmpty) None
              else {
                val source = SourceFile(cls.associatedFile, Array())
                val unit = mkCompilationUnit(source, cls.tree, forceTrees = true)
                unit.pickled += (cls -> unpickler.unpickler.bytes)
                Some(unit)
              }
            case tree: Tree[_] =>
               alreadyLoaded()
            case _ =>
              cannotUnpickle(s"its class file does not have a TASTY attribute")
          }
        case _ => None
      }

      // The TASTY section in a/b/C.class may either contain a class a.b.C, an object a.b.C, or both.
      // We first try to load the class and fallback to loading the object if the class doesn't exist.
      // Note that if both the class and the object are present, then loading the class will also load
      // the object, this is why we use orElse here, otherwise we could load the object twice and
      // create ambiguities!
      ctx.base.staticRef(className) match {
        case clsd: ClassDenotation =>
          clsd.infoOrCompleter match {
            case info: ClassfileLoader =>
              info.load(clsd) // sets cls.treeOrProvider and cls.moduleClass.treeProvider as a side-effect
            case _ =>
          }
          def moduleClass = clsd.owner.info.member(className.moduleClassName).symbol
          compilationUnit(clsd.classSymbol).orElse(compilationUnit(moduleClass))
        case _ =>
          cannotUnpickle(s"no class file was found")
      }
  }
}
