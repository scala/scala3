package dotty.tools
package dotc
package transform

import core._
import Contexts._
import Symbols._
import Flags._
import Names._
import NameOps._
import Decorators._
import TypeUtils._
import Types._
import util.Spans.Span
import config.Printers.transforms

/** A utility class for generating access proxies. Currently used for
 *  inline accessors and protected accessors.
 */
abstract class AccessProxies {
  import ast.tpd._
  import AccessProxies._

  /** accessor -> accessed */
  private val accessedBy = MutableSymbolMap[Symbol]()

  /** Given the name of an accessor, is the receiver of the call to accessed obtained
   *  as a parameterer?
   */
  protected def passReceiverAsArg(accessorName: Name)(using Context): Boolean = false

  /** The accessor definitions that need to be added to class `cls` */
  private def accessorDefs(cls: Symbol)(using Context): Iterator[DefDef] =
    for accessor <- cls.info.decls.iterator; accessed <- accessedBy.get(accessor) yield
      DefDef(accessor.asTerm, prefss => {
        def numTypeParams = accessed.info match {
          case info: PolyType => info.paramNames.length
          case _ => 0
        }
        val (targs, argss) = splitArgs(prefss)
        val (accessRef, forwardedTpts, forwardedArgss) =
          if (passReceiverAsArg(accessor.name))
            (argss.head.head.select(accessed), targs.takeRight(numTypeParams), argss.tail)
          else
            (if (accessed.isStatic) ref(accessed) else ref(TermRef(cls.thisType, accessed)),
             targs, argss)
        val rhs =
          if (accessor.name.isSetterName &&
              forwardedArgss.nonEmpty && forwardedArgss.head.nonEmpty) // defensive conditions
            accessRef.becomes(forwardedArgss.head.head)
          else
            accessRef
              .appliedToTypeTrees(forwardedTpts)
              .appliedToArgss(forwardedArgss)
              .etaExpandCFT(using ctx.withOwner(accessor))
        rhs.withSpan(accessed.span)
      })

  /** Add all needed accessors to the `body` of class `cls` */
  def addAccessorDefs(cls: Symbol, body: List[Tree])(using Context): List[Tree] = {
    val accDefs = accessorDefs(cls)
    transforms.println(i"add accessors for $cls: $accDefs%, %")
    if (accDefs.isEmpty) body else body ++ accDefs
  }

  trait Insert {
    import ast.tpd._

    /** The name of the accessor for definition with given `name` in given `site` */
    def accessorNameOf(name: TermName, site: Symbol)(using Context): TermName
    def needsAccessor(sym: Symbol)(using Context): Boolean

    def ifNoHost(reference: RefTree)(using Context): Tree = {
      assert(false, "no host found for $reference with ${reference.symbol.showLocated} from ${ctx.owner}")
      reference
    }

    /** A fresh accessor symbol */
    private def newAccessorSymbol(owner: Symbol, name: TermName, info: Type, span: Span)(using Context): TermSymbol = {
      val sym = newSymbol(owner, name, Synthetic | Method, info, coord = span).entered
      if (sym.allOverriddenSymbols.exists(!_.is(Deferred))) sym.setFlag(Override)
      sym
    }

    /** An accessor symbol, create a fresh one unless one exists already */
    protected def accessorSymbol(owner: Symbol, accessorName: TermName, accessorInfo: Type, accessed: Symbol)(using Context): Symbol = {
      def refersToAccessed(sym: Symbol) = accessedBy.get(sym).contains(accessed)
      owner.info.decl(accessorName).suchThat(refersToAccessed).symbol.orElse {
        val acc = newAccessorSymbol(owner, accessorName, accessorInfo, accessed.span)
        accessedBy(acc) = accessed
        acc
      }
    }

    /** Rewire reference to refer to `accessor` symbol */
    private def rewire(reference: RefTree, accessor: Symbol)(using Context): Tree = {
      reference match {
        case Select(qual, _) if qual.tpe.derivesFrom(accessor.owner) => qual.select(accessor)
        case _ => ref(accessor)
      }
    }.withSpan(reference.span)

    /** Given a reference to a getter accessor, the corresponding setter reference */
    def useSetter(getterRef: Tree)(using Context): Tree = getterRef match {
      case getterRef: RefTree =>
        val getter = getterRef.symbol.asTerm
        val accessed = accessedBy(getter)
        val setterName = getter.name.setterName
        def toSetterInfo(getterInfo: Type): Type = getterInfo match {
          case getterInfo: LambdaType =>
            getterInfo.derivedLambdaType(resType = toSetterInfo(getterInfo.resType))
          case _ =>
            MethodType(getterInfo :: Nil, defn.UnitType)
        }
        val setterInfo = toSetterInfo(getter.info.widenExpr)
        val setter = accessorSymbol(getter.owner, setterName, setterInfo, accessed)
        rewire(getterRef, setter)
      case Apply(fn, args) =>
        cpy.Apply(getterRef)(useSetter(fn), args)
      case TypeApply(fn, args) =>
        cpy.TypeApply(getterRef)(useSetter(fn), args)
    }

    /** Create an accessor unless one exists already, and replace the original
      *  access with a reference to the accessor.
      *
      *  @param reference    The original reference to the non-public symbol
      *  @param onLHS        The reference is on the left-hand side of an assignment
      */
    def useAccessor(reference: RefTree)(using Context): Tree = {
      val accessed = reference.symbol.asTerm
      var accessorClass = hostForAccessorOf(accessed: Symbol)
      if (accessorClass.exists) {
        if accessorClass.is(Package) then
          accessorClass = ctx.owner.topLevelClass
        val accessorName = accessorNameOf(accessed.name, accessorClass)
        val accessorInfo =
          accessed.info.ensureMethodic.asSeenFrom(accessorClass.thisType, accessed.owner)
        val accessor = accessorSymbol(accessorClass, accessorName, accessorInfo, accessed)
        rewire(reference, accessor)
      }
      else ifNoHost(reference)
    }

    /** Replace tree with a reference to an accessor if needed */
    def accessorIfNeeded(tree: Tree)(using Context): Tree = tree match {
      case tree: RefTree if needsAccessor(tree.symbol) =>
        if (tree.symbol.isConstructor) {
          report.error("Implementation restriction: cannot use private constructors in inlineable methods", tree.srcPos)
          tree // TODO: create a proper accessor for the private constructor
        }
        else useAccessor(tree)
      case _ =>
        tree
    }
  }
}
object AccessProxies {
  /** Where an accessor for the `accessed` symbol should be placed.
   *  This is the closest enclosing class that has `accessed` as a member.
   */
  def hostForAccessorOf(accessed: Symbol)(using Context): Symbol = {
    def recur(cls: Symbol): Symbol =
      if (!cls.exists) NoSymbol
      else if cls.derivesFrom(accessed.owner)
              || cls.companionModule.moduleClass == accessed.owner
      then cls
      else recur(cls.owner)
    recur(ctx.owner)
  }
}
