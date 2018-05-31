package dotty.tools.dotc
package transform

import core._
import Contexts.Context
import Symbols._
import Flags._
import Names._
import NameOps._
import Decorators._
import TypeUtils._
import Annotations.Annotation
import Types._
import NameKinds.ClassifiedNameKind
import ast.Trees._
import util.Property
import util.Positions.Position
import config.Printers.transforms

/** A utility class for generating access proxies. Currently used for
 *  inline accessors and protected accessors.
 */
abstract class AccessProxies {
  import ast.tpd._
  import AccessProxies._

  /** accessor -> accessed */
  private val accessedBy = newMutableSymbolMap[Symbol]

  /** The accessor definitions that need to be added to class `cls`
   *  As a side-effect, this method removes entries from the `accessedBy` map.
   *  So a second call of the same method will yield the empty list.
   */
  private def accessorDefs(cls: Symbol)(implicit ctx: Context): Iterator[DefDef] =
    for (accessor <- cls.info.decls.iterator; accessed <- accessedBy.remove(accessor)) yield
      polyDefDef(accessor.asTerm, tps => argss => {
        val accessRef = ref(TermRef(cls.thisType, accessed))
        val rhs =
          if (accessor.name.isSetterName &&
              argss.nonEmpty && argss.head.nonEmpty) // defensive conditions
            accessRef.becomes(argss.head.head)
          else
            accessRef.appliedToTypes(tps).appliedToArgss(argss)
        rhs.withPos(accessed.pos)
      })

  /** Add all needed accessors to the `body` of class `cls` */
  def addAccessorDefs(cls: Symbol, body: List[Tree])(implicit ctx: Context): List[Tree] = {
    val accDefs = accessorDefs(cls)
    transforms.println(i"add accessors for $cls: $accDefs%, %")
    if (accDefs.isEmpty) body else body ++ accDefs
  }

  trait Insert {
    import ast.tpd._

    def accessorNameKind: ClassifiedNameKind
    def needsAccessor(sym: Symbol)(implicit ctx: Context): Boolean

    /** A fresh accessor symbol */
    def newAccessorSymbol(owner: Symbol, name: TermName, info: Type, pos: Position)(implicit ctx: Context): TermSymbol = {
      val sym = ctx.newSymbol(owner, name, Synthetic | Method, info, coord = pos).entered
      if (sym.allOverriddenSymbols.exists(!_.is(Deferred))) sym.setFlag(Override)
      sym
    }

    private def rewire(reference: RefTree, accessor: Symbol)(implicit ctx: Context): Tree = {
      reference match {
        case Select(qual, _) => qual.select(accessor)
        case Ident(name) => ref(accessor)
      }
    }.withPos(reference.pos)

    private def isAccessor(sym: Symbol, accessed: Symbol) = accessedBy.get(sym) == Some(accessed)

    def useSetter(getterRef: RefTree)(implicit ctx: Context): Tree = {
      val getter = getterRef.symbol
      val accessed = accessedBy(getter)
      val accessedName = accessed.name.asTermName
      val setterName = accessorNameKind(accessedName.setterName)
      val setter =
        getter.owner.info.decl(setterName).suchThat(isAccessor(_, accessed)).symbol.orElse {
          val setterInfo = MethodType(getter.info.widenExpr :: Nil, defn.UnitType)
          val setter = newAccessorSymbol(getter.owner, setterName, setterInfo, getter.pos)
          accessedBy(setter) = accessed
          setter
        }
      rewire(getterRef, setter)
    }

    /** Create an accessor unless one exists already, and replace the original
      *  access with a reference to the accessor.
      *
      *  @param reference    The original reference to the non-public symbol
      *  @param onLHS        The reference is on the left-hand side of an assignment
      */
    def useAccessor(reference: RefTree, onLHS: Boolean)(implicit ctx: Context): Tree = {
      assert(!onLHS)

      val accessed = reference.symbol.asTerm


      var accessorClass = hostForAccessorOf(accessed: Symbol)
      if (!accessorClass.exists) {
        val curCls = ctx.owner.enclosingClass
        transforms.println(i"${curCls.ownersIterator.toList}%, %")
        ctx.error(i"illegal access to protected ${accessed.showLocated} from $curCls",
          reference.pos)
        accessorClass = curCls
      }

      val accessorName = accessorNameKind(
        if (onLHS) accessed.name.setterName else accessed.name)


      val accessor =
        accessorClass.info.decl(accessorName).suchThat(isAccessor(_, accessed)).symbol.orElse {
          val accessorRawInfo =
            if (onLHS) MethodType(accessed.info :: Nil, defn.UnitType)
            else accessed.info.ensureMethodic
          val accessorInfo =
            accessorRawInfo.asSeenFrom(accessorClass.thisType, accessed.owner)

          val acc = newAccessorSymbol(accessorClass, accessorName, accessorInfo, accessed.pos)
          accessedBy(acc) = accessed
          acc
        }
      rewire(reference, accessor)
    }

    /** Replace tree with a reference to an accessor if needed */
    def accessorIfNeeded(tree: Tree)(implicit ctx: Context): Tree = tree match {
      case tree: RefTree if needsAccessor(tree.symbol) =>
        if (tree.symbol.isConstructor) {
          ctx.error("Implementation restriction: cannot use private constructors in inline methods", tree.pos)
          tree // TODO: create a proper accessor for the private constructor
        }
        else useAccessor(tree, onLHS = false)
      case Assign(lhs: RefTree, rhs) if needsAccessor(lhs.symbol) =>
        cpy.Apply(tree)(useAccessor(lhs, onLHS = true), List(rhs))
      case _ =>
        tree
    }
  }
}
object AccessProxies {
  def hostForAccessorOf(accessed: Symbol)(implicit ctx: Context): Symbol =
    ctx.owner.ownersIterator.findSymbol(_.derivesFrom(accessed.owner))
}
