package dotty.tools.dotc
package transform
package init

import core._
import Flags._
import Contexts._
import Types._
import Symbols._
import Decorators._
import printing.SyntaxHighlighting
import reporting.trace
import config.Printers.init


import ast.tpd._

import scala.collection.mutable



/** The dependencies of a static object or a class
 *
 *  This class is used in checking cyclic initialization of static objects.
 *
 *  For the check to be fast, the algorithm uses a combination of
 *  coarse-grained approximation and fine-grained abstractions.
 *
 *  Fine-grained abstractions are created from the initialization
 *  check for static objects.
 *
 *  Coarse-grained abstractions are constructed as follows:
 *
 *  - if a static object `O` is used in another class/static-object `B`,
 *    then B -> O
 *  - if `new C` appears in a another class/static-object `B`,
 *    then B -> C
 *  - if a static-object/class `A` extends another class `B`,
 *    then A -> B
 *
 *  Given a dependency graph, we check if there exists cycles.
 *
 *  This check does not need to care about objects in libraries, as separate
 *  compilation ensures that there cannot be cyles between two separately
 *  compiled projects.
 *
 */
case class Dependency(sym: Symbol)(val source: Tree) {
  def show(using Context): String = "Dep(" + sym.show + ")"
}

class CycleChecker {
  private val summaryCache = mutable.Map.empty[Symbol, List[Dependency]]

  val classesInCurrentRun = mutable.Set.empty[Symbol]
  val objectsInCurrentRun = mutable.Set.empty[Symbol]

  /** Checking state */
  case class State(visited: mutable.Set[Symbol], path: Vector[Tree])

  def cacheObjectDependencies(obj: Symbol, deps: List[Dependency]): Unit =
    objectsInCurrentRun += obj
    summaryCache(obj) = deps

  def dependenciesOf(sym: Symbol)(using Context): List[Dependency] =
    if (summaryCache.contains(sym)) summaryCache(sym)
    else if (!classesInCurrentRun.contains(sym)) Nil
    else trace("summary for " + sym.show, init) {
      assert(sym.isClass)
      val cls = sym.asClass
      val deps = analyze(cls)
      summaryCache(cls) = deps
      deps
    }

  def check()(using Context): Unit = ???

  def clean() = {
    summaryCache.clear()
    classesInCurrentRun.clear()
    objectsInCurrentRun.clear()
  }

  def analyze(cls: ClassSymbol)(using Context): List[Dependency] = {
    def isStaticObjectRef(sym: Symbol) =
      sym.isTerm && !sym.is(Flags.Package) && sym.is(Flags.Module) && sym.isStatic

    val isStaticObj = isStaticObjectRef(cls)

    if (cls.defTree.isEmpty) return Nil

    val cdef = cls.defTree.asInstanceOf[TypeDef]
    val tpl = cdef.rhs.asInstanceOf[Template]

    var deps: List[Dependency] = Nil

    def analyzeType(tp: Type, source: Tree): Unit = tp match {
      case (_: ConstantType) | NoPrefix =>

      case tmref: TermRef if isStaticObjectRef(tmref.symbol) =>
        deps = Dependency(tmref.symbol)(source) :: deps

      case tmref: TermRef =>
        analyzeType(tmref.prefix, source)

      case ThisType(tref) =>
        if isStaticObjectRef(tref.symbol.sourceModule) && tref.symbol != cls
        then
          val obj = tref.symbol.sourceModule
          deps = Dependency(obj)(source) :: deps

      case SuperType(thisTp, _) =>
        analyzeType(thisTp, source)

      case _: TypeRef | _: AppliedType =>
        // possible from parent list

      case _ =>
        throw new Exception("unexpected type: " + tp)
    }


    val traverser = new TreeTraverser {
      override def traverse(tree: Tree)(using Context): Unit =
        tree match {
          case tree @ Template(_, parents, self, _) =>
            if !isStaticObj then
              parents.foreach(traverse(_))
            tree.body.foreach {
              case ddef: DefDef =>
                traverse(ddef)
              case vdef: ValDef if vdef.symbol.is(Flags.Lazy) =>
                traverse(vdef)
              case stat =>
                if !isStaticObj then traverse(stat)
            }

          case tree: RefTree if tree.isTerm =>
            analyzeType(tree.tpe, tree)

          case tree: This =>
            analyzeType(tree.tpe, tree)

          case tree: ValOrDefDef =>
            traverseChildren(tree.rhs)

          case tdef: TypeDef =>
            // don't go into nested classes

          case tree: New =>
            deps = Dependency(tree.tpe.classSymbol)(tree) :: deps

          case _ =>
            traverseChildren(tree)
        }
    }

    // TODO: the traverser might create duplicate entries for parents
    tpl.parents.foreach { tree =>
      val tp = tree.tpe
      deps = Dependency(tp.classSymbol)(tree) :: deps
    }

    traverser.traverse(tpl)
    deps
  }

}
