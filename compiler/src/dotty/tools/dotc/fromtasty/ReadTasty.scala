package dotty.tools
package dotc
package fromtasty

import core.*
import Decorators.*
import Contexts.*
import Symbols.{Symbol, ClassSymbol}
import SymDenotations.ClassDenotation
import Denotations.staticRef
import NameOps.*
import ast.Trees.Tree
import Phases.Phase
import core.tasty.Attributes

/** Load trees from TASTY files */
class ReadTasty extends Phase {

  def phaseName: String = "readTasty"

  override def isRunnable(using Context): Boolean =
    ctx.settings.fromTasty.value

  override def runOn(units: List[CompilationUnit])(using Context): List[CompilationUnit] =
    withMode(Mode.ReadPositions) {
      val nextUnits = collection.mutable.ListBuffer.empty[CompilationUnit]
      val unitContexts = units.view.map(ctx.fresh.setCompilationUnit)
      for unitContext <- unitContexts if addTasty(nextUnits += _)(using unitContext) do ()
      nextUnits.toList
    }

  def addTasty(fn: CompilationUnit => Unit)(using Context): Boolean = monitor(phaseName):
    readTASTY(ctx.compilationUnit).foreach(fn)

  def readTASTY(unit: CompilationUnit)(using Context): Option[CompilationUnit] = unit match {
    case unit: TASTYCompilationUnit =>
      val className = unit.className.toTypeName

      def cannotUnpickle(reason: String): None.type = {
        report.error(em"class $className cannot be unpickled because $reason")
        None
      }

      def compilationUnit(cls: Symbol): Option[CompilationUnit] = cls match {
        case cls: ClassSymbol =>
          (cls.rootTreeOrProvider: @unchecked) match {
            case unpickler: tasty.DottyUnpickler =>
              if (cls.rootTree.isEmpty) None
              else {
                val attributes = unpickler.tastyAttributes
                if attributes.isJava && !ctx.settings.XjavaTasty.value then
                  // filter out Java compilation units if -Xjava-tasty is not set
                  None
                else if attributes.isOutline && !ctx.settings.XallowOutlineFromTasty.value then
                  cannotUnpickle("it contains outline signatures and -Xallow-outline-from-tasty is not set.")
                else
                  val unit = CompilationUnit(cls, cls.rootTree, forceTrees = true)
                  unit.pickled += (cls -> (() => unpickler.unpickler.bytes))
                  Some(unit)
              }
            case tree: Tree[?] =>
              // TODO handle correctly this case correctly to get the tree or avoid it completely.
              cls.denot.infoOrCompleter match {
                case _ => Some(AlreadyLoadedCompilationUnit(cls.denot.fullName.toString))
              }
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
      staticRef(className) match {
        case clsd: ClassDenotation =>
          clsd.infoOrCompleter match {
            case info: TastyLoader =>
              info.doComplete(clsd) // sets cls.rootTreeOrProvider and cls.moduleClass.treeProvider as a side-effect
            case _ =>
          }
          def moduleClass = clsd.owner.info.member(className.moduleClassName).symbol
          compilationUnit(clsd.classSymbol).orElse(compilationUnit(moduleClass))
        case _ =>
          staticRef(className.moduleClassName) match {
            case clsd: ClassDenotation =>
              compilationUnit(clsd.classSymbol)
            case denot =>
              cannotUnpickle(s"no class file was found for denot: $denot")
          }
      }
    case unit =>
      Some(unit)
  }

  protected def run(using Context): Unit = unsupported("run")
}
