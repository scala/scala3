package dotty.tools.dotc
package transform

import TreeTransforms._
import core.DenotTransformers._
import core.Symbols._
import core.Contexts._
import core.Types._
import core.Flags._
import core.Decorators._
import core.StdNames.nme
import core.Names._
import ast.Trees._
import SymUtils._
import util.Attachment
import collection.mutable

/** This phase decorates News and parent constructors of non-static inner classes
 *  with an attachment indicating the outer reference as a tree. This is necessary because
 *  outer prefixes are erased, and explicit outer runs only after erasure.
 */
class OuterAccessors extends MiniPhaseTransform with InfoTransformer { thisTransformer =>
  import OuterAccessors._
  import ast.tpd._

  val Outer = new Attachment.Key[Tree]

  override def phaseName: String = "outerAccessors"

  override def treeTransformPhase = thisTransformer.next

  /** Add outer accessors if a class always needs an outer pointer */
  override def transformInfo(tp: Type, sym: Symbol)(implicit ctx: Context) = tp match {
    case tp @ ClassInfo(_, cls, _, decls, _) if needsOuterAlways(cls) =>
      val newDecls = decls.cloneScope
      newOuterAccessors(cls).foreach(newDecls.enter)
      tp.derivedClassInfo(decls = newDecls)
    case _ =>
      tp
  }

  /** A new outer accessor or param accessor */
  private def newOuterSym(owner: ClassSymbol, cls: ClassSymbol, name: TermName, flags: FlagSet)(implicit ctx: Context) = {
    ctx.newSymbol(owner, name, Synthetic | flags, cls.owner.enclosingClass.typeRef, coord = cls.coord)
  }

  /** A new outer accessor for class `cls` which is a member of `owner` */
  private def newOuterAccessor(owner: ClassSymbol, cls: ClassSymbol)(implicit ctx: Context) = {
    val deferredIfTrait = if (cls.is(Trait)) Deferred else EmptyFlags
    newOuterSym(owner, cls, cls.outerAccName, Final | Stable | deferredIfTrait)
  }

  /** A new param accessor for the outer field in class `cls` */
  private def newOuterParamAccessor(cls: ClassSymbol)(implicit ctx: Context) =
    newOuterSym(cls, cls, nme.OUTER, Private | ParamAccessor)

  /** The outer accessor and potentially outer param accessor needed for class `cls` */
  private def newOuterAccessors(cls: ClassSymbol)(implicit ctx: Context) =
    newOuterAccessor(cls, cls) :: (if (cls is Trait) Nil else newOuterParamAccessor(cls) :: Nil)

  /** First, add outer accessors if a class does not have them yet and it references an outer this.
   *  If the class has outer accessors, implement them.
   *  Furthermore, if a parent trait might have outer accessors (decided by needsOuterIfReferenced),
   *  provide an implementation for the outer accessor by computing the parent's
   *  outer from the parent type prefix. If the trait ends up not having an outer accessor
   *  after all, the implementation is redundant, but does not harm.
   *  The same logic is not done for non-trait parent classes because for them the outer
   *  pointer is passed in the super constructor, which will be implemented later in
   *  a separate phase which needs to run after erasure. However, we make sure here
   *  that the super class constructor is indeed a New, and not just a type.
   */
  override def transformTemplate(impl: Template)(implicit ctx: Context, info: TransformerInfo): Tree = {
    val cls = ctx.owner.asClass
    val isTrait = cls.is(Trait)
    if (needsOuterIfReferenced(cls) && !needsOuterAlways(cls) && referencesOuter(cls, impl))
      newOuterAccessors(cls).foreach(_.enteredAfter(thisTransformer))
    if (hasOuter(cls)) {
      val outerAcc = cls.info.member(cls.outerAccName).symbol.asTerm
      val newDefs = new mutable.ListBuffer[Tree]
      if (isTrait)
        newDefs += DefDef(outerAcc, EmptyTree)
      else {
        val outerParamAcc = outerParamAccessor(cls).asTerm
        newDefs += ValDef(outerParamAcc, EmptyTree)
        newDefs += DefDef(outerAcc, ref(outerParamAcc))
      }
      val parents1 =
        for (parent <- impl.parents) yield {
          val parentCls = parent.tpe.classSymbol.asClass
          if (parentCls.is(Trait)) {
            if (needsOuterIfReferenced(parentCls)) {
              val outerAccImpl = newOuterAccessor(cls, parentCls).enteredAfter(thisTransformer)
              newDefs += DefDef(outerAccImpl, singleton(outerPrefix(parent.tpe)))
            }
            parent
          }
          else parent match { // ensure class parent is a constructor
            case parent: TypeTree => New(parent.tpe, Nil).withPos(impl.pos)
            case _ => parent
          }
        }
      cpy.Template(impl)(parents = parents1, body = impl.body ++ newDefs)
    }
    else impl
  }
}

object OuterAccessors {
  import ast.tpd._

  private val LocalInstantiationSite = Module | Private

  /** Class needs an outer pointer, provided there is a reference to an outer this in it. */
  def needsOuterIfReferenced(cls: ClassSymbol)(implicit ctx: Context): Boolean = !(
    cls.isStatic ||
    cls.owner.enclosingClass.isStaticOwner ||
    cls.is(Interface)
  )

  /** Class unconditionally needs an outer pointer. This is the case if
   *  the class needs an outer pointer if referenced and one of the following holds:
   *  - we might not know at all instantiation sites whether outer is referenced or not
   *  - we need to potentially pass along outer to a parent class or trait
   */
  def needsOuterAlways(cls: ClassSymbol)(implicit ctx: Context): Boolean =
    needsOuterIfReferenced(cls) &&
    (!hasLocalInstantiation(cls) || // needs outer because we might not know whether outer is referenced or not
     cls.classInfo.parents.exists(parent => // needs outer to potentially pass along to parent
       needsOuterIfReferenced(parent.classSymbol.asClass)))

  /** Class is always instantiated in the compilation unit where it is defined */
  def hasLocalInstantiation(cls: ClassSymbol)(implicit ctx: Context): Boolean =
    cls.owner.isTerm || cls.is(LocalInstantiationSite)

  /** The outer parameter accessor of cass `cls` */
  def outerParamAccessor(cls: ClassSymbol)(implicit ctx: Context) =
    cls.info.decl(nme.OUTER).symbol

  /** Class has outer accessor. Can be called only after phase OuterAccessors. */
  def hasOuter(cls: ClassSymbol)(implicit ctx: Context): Boolean =
    cls.info.decl(cls.outerAccName).exists

  /** Template `impl` of class `cls` references an outer this which goes to
   *  a class that is not a static owner.
   */
  def referencesOuter(cls: ClassSymbol, impl: Template)(implicit ctx: Context): Boolean =
    existsSubTreeOf(impl) {
      case thisTree @ This(_) =>
        val thisCls = thisTree.symbol
        thisCls != cls && !thisCls.isStaticOwner && cls.isContainedIn(thisCls)
      case _ =>
        false
    }

  /** The outer prefix implied by type `tpe` */
  def outerPrefix(tpe: Type)(implicit ctx: Context): Type = tpe match {
    case tpe: TypeRef =>
      tpe.symbol match {
        case cls: ClassSymbol =>
          if (tpe.prefix eq NoPrefix) cls.owner.enclosingClass.thisType
          else tpe.prefix
        case _ =>
          outerPrefix(tpe.underlying)
      }
    case tpe: TypeProxy =>
      outerPrefix(tpe.underlying)
  }

  /** If `cls` has an outer parameter add one to the method type `tp`. */
  def addOuterParam(cls: ClassSymbol, tp: Type)(implicit ctx: Context): Type =
    if (hasOuter(cls)) {
      val mt @ MethodType(pnames, ptypes) = tp
      mt.derivedMethodType(
        nme.OUTER :: pnames, cls.owner.enclosingClass.typeRef :: ptypes, mt.resultType)
    }
    else tp

  /** If function in an apply node is a constructor that needs to be passed an
   *  outer argument, the singleton list with the argument, otherwise Nil.
   */
  def outerArgs(fun: Tree)(implicit ctx: Context): List[Tree] = {
    if (fun.symbol.isConstructor) {
      val cls = fun.symbol.owner.asClass
      def outerArg(receiver: Tree): Tree = receiver match {
        case New(tpt) => TypeTree(outerPrefix(tpt.tpe)).withPos(tpt.pos)
        case This(_) => ref(outerParamAccessor(cls))
        case TypeApply(Select(r, nme.asInstanceOf_), args) => outerArg(r) // cast was inserted, skip
      }
      if (hasOuter(cls))
        methPart(fun) match {
          case Select(receiver, _) => outerArg(receiver) :: Nil
        }
      else Nil
    }
    else Nil
  }
}