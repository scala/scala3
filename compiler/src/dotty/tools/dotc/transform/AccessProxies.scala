package dotty.tools
package dotc
package transform

import core.*
import Contexts.*
import Symbols.*
import Flags.*
import Names.*
import NameOps.*
import Decorators.*
import Types.*
import util.Spans.Span
import config.Printers.transforms
import Annotations.ExperimentalAnnotation

/** A utility class for generating access proxies. Currently used for
 *  inline accessors and protected accessors.
 */
abstract class AccessProxies {
  import ast.tpd.*
  import AccessProxies.*

  /** accessor -> accessed */
  private val accessedBy = MutableSymbolMap[Symbol]()

  /** Given the name of an accessor, is the receiver of the call to accessed obtained
   *  as a parameterer?
   */
  protected def passReceiverAsArg(accessorName: Name)(using Context): Boolean = false

  /** The accessor definitions that need to be added to class `cls` */
  private def accessorDefs(cls: Symbol)(using Context): Iterator[DefDef] =
    for accessor <- cls.info.decls.iterator; accessed <- accessedBy.get(accessor) yield
      accessorDef(accessor, accessed)

  protected def accessorDef(accessor: Symbol, accessed: Symbol)(using Context): DefDef =
    DefDef(accessor.asTerm,
      prefss => {
        def numTypeParams = accessed.info match {
          case info: PolyType => info.paramNames.length
          case _ => 0
        }
        val (targs, argss) = splitArgs(prefss)
        val (accessRef, forwardedTpts, forwardedArgss) =
          if (passReceiverAsArg(accessor.name))
            (argss.head.head.select(accessed), targs.takeRight(numTypeParams), argss.tail)
          else
            (if (accessed.isStatic) ref(accessed) else ref(TermRef(accessor.owner.thisType, accessed)),
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
      }
    )

  /** Add all needed accessors to the `body` of class `cls` */
  def addAccessorDefs(cls: Symbol, body: List[Tree])(using Context): List[Tree] = {
    val accDefs = accessorDefs(cls).toList
    transforms.println(i"add accessors for $cls: $accDefs%, %")
    if (accDefs.isEmpty) body else body ++ accDefs
  }

  trait Insert {
    import ast.tpd.*

    /** The name of the accessor for definition with given `name` in given `site` */
    def accessorNameOf(name: TermName, site: Symbol)(using Context): TermName
    def needsAccessor(sym: Symbol)(using Context): Boolean

    def ifNoHost(reference: RefTree)(using Context): Tree = {
      assert(false, i"no host found for $reference with ${reference.symbol.showLocated} from ${ctx.owner}")
      reference
    }

    /** A fresh accessor symbol */
    private def newAccessorSymbol(owner: Symbol, name: TermName, info: Type, accessed: Symbol)(using Context): TermSymbol = {
      val sym = newSymbol(owner, name, Synthetic | Method, info, coord = accessed.span).entered
      if accessed.is(Private) then sym.setFlag(Final)
      else if sym.allOverriddenSymbols.exists(!_.is(Deferred)) then sym.setFlag(Override)
      ExperimentalAnnotation.copy(accessed).foreach(sym.addAnnotation)
      sym
    }

    /** An accessor symbol, create a fresh one unless one exists already */
    protected def accessorSymbol(owner: Symbol, accessorName: TermName, accessorInfo: Type, accessed: Symbol)(using Context): Symbol = {
      def refersToAccessed(sym: Symbol) = accessedBy.get(sym).contains(accessed)
      owner.info.decl(accessorName).suchThat(refersToAccessed).symbol.orElse {
        val acc = newAccessorSymbol(owner, accessorName, accessorInfo, accessed)
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
      */
    def useAccessor(reference: RefTree)(using Context): Tree = {
      val accessed = reference.symbol.asTerm
      var accessorClass = hostForAccessorOf(accessed: Symbol)
      if (accessorClass.exists) {
        if accessorClass.is(Package) then
          accessorClass = ctx.owner.topLevelClass
        val accessorName = accessorNameOf(accessed.name, accessorClass)
        val mappedInfo = accessed.info match
          // TypeRef pointing to module class seems to not be stable, so we remap that to a TermRef
          // see test i22593.scala (and issue #i22593)
          case tref @ TypeRef(prefix, _) if tref.symbol.is(Module) => TermRef(prefix, tref.symbol.companionModule)
          case other => other
        val accessorInfo =
          mappedInfo.ensureMethodic.asSeenFrom(accessorClass.thisType, accessed.owner)
        val accessor = accessorSymbol(accessorClass, accessorName, accessorInfo, accessed)
        rewire(reference, accessor)
      }
      else ifNoHost(reference)
    }

    /** Replace tree with a reference to an accessor if needed */
    def accessorIfNeeded(tree: Tree)(using Context): Tree = tree match {
      case tree: RefTree if needsAccessor(tree.symbol) =>
        if (tree.symbol.isConstructor) {
          report.error("Cannot use private constructors in inline methods. You can use @publicInBinary to make constructor accessible in inline methods.", tree.srcPos)
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
