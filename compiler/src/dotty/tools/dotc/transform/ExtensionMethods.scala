/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */
package dotty.tools.dotc
package transform

import dotty.tools.dotc.transform.MegaPhase._
import ValueClasses._
import dotty.tools.dotc.ast.{Trees, tpd}
import scala.collection.{ mutable, immutable }
import mutable.ListBuffer
import core._
import dotty.tools.dotc.core.Phases.{NeedsCompanions, Phase}
import Types._, Contexts._, Constants._, Names._, NameOps._, Flags._, DenotTransformers._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, Scopes._, Denotations._
import TypeErasure.{ valueErasure, ErasedValueType }
import TypeUtils._
import NameKinds.{ExtMethName, UniqueExtMethName}
import util.Positions._
import Decorators._
import SymUtils._

/**
 * Perform Step 1 in the inline classes SIP: Creates extension methods for all
 * methods in a value class, except parameter or super accessors, or constructors.
 *
 * Additionally, for a value class V, let U be the underlying type after erasure. We add
 * to the companion module of V two cast methods:
 *   def u2evt$(x0: U): ErasedValueType(V, U)
 *   def evt2u$(x0: ErasedValueType(V, U)): U
 * The casts are used in [[Erasure]] to make it typecheck, they are then removed
 * in [[ElimErasedValueType]].
 * This is different from the implementation of value classes in Scala 2
 * (see SIP-15) which uses `asInstanceOf` which does not typecheck.
 *
 * Finally, if the constructor of a value class is private pr protected
 * it is widened to public.
 */
class ExtensionMethods extends MiniPhase with DenotTransformer with FullParameterization { thisPhase =>

  import tpd._
  import ExtensionMethods._

  /** the following two members override abstract members in Transform */
  override def phaseName: String = "extmethods"

  override def runsAfter: Set[Class[_ <: Phase]] = Set(classOf[ElimRepeated])

  override def runsAfterGroupsOf = Set(classOf[FirstTransform]) // need companion objects to exist

  override def changesMembers = true // the phase adds extension methods

  override def transform(ref: SingleDenotation)(implicit ctx: Context): SingleDenotation = ref match {
    case moduleClassSym: ClassDenotation if moduleClassSym is ModuleClass =>
      moduleClassSym.linkedClass match {
        case valueClass: ClassSymbol if isDerivedValueClass(valueClass) =>
          val cinfo = moduleClassSym.classInfo
          val decls1 = cinfo.decls.cloneScope
          val moduleSym = moduleClassSym.symbol.asClass

          var newSuperClass: Type = null

          ctx.atPhase(thisPhase.next) { implicit ctx =>
            // In Scala 2, extension methods are added before pickling so we should
            // not generate them again.
            if (!(valueClass is Scala2x)) ctx.atPhase(thisPhase) { implicit ctx =>
              for (decl <- valueClass.classInfo.decls) {
                if (isMethodWithExtension(decl)) {
                  val meth = createExtensionMethod(decl, moduleClassSym.symbol)
                  decls1.enter(meth)
                  // Workaround #1895: force denotation of `meth` to be
                  // at phase where `meth` is entered into the decls of a class
                  meth.denot(ctx.withPhase(thisPhase.next))
                }
              }
            }

            val underlying = valueErasure(underlyingOfValueClass(valueClass))
            val evt = ErasedValueType(valueClass.typeRef, underlying)
            val u2evtSym = ctx.newSymbol(moduleSym, nme.U2EVT, Synthetic | Method,
              MethodType(List(nme.x_0), List(underlying), evt))
            val evt2uSym = ctx.newSymbol(moduleSym, nme.EVT2U, Synthetic | Method,
              MethodType(List(nme.x_0), List(evt), underlying))

            val defn = ctx.definitions

            val underlyingCls = underlying.classSymbol
            val underlyingClsName =
              if (underlyingCls.isNumericValueClass || underlyingCls == defn.BooleanClass) underlyingCls.name
              else nme.Object

            val syp = ctx.requiredClass(s"dotty.runtime.vc.VC${underlyingClsName}Companion").asClass

            newSuperClass = tpd.ref(syp).select(nme.CONSTRUCTOR).appliedToType(valueClass.typeRef).tpe.resultType

            decls1.enter(u2evtSym)
            decls1.enter(evt2uSym)
          }

          // Add the extension methods, the cast methods u2evt$ and evt2u$, and a VC*Companion superclass
          moduleClassSym.copySymDenotation(info =
            cinfo.derivedClassInfo(
              // FIXME: use of VC*Companion superclasses is disabled until the conflicts with SyntheticMethods are solved.
              //classParents = List(newSuperClass)
              decls = decls1))
        case _ =>
          moduleClassSym
      }
    case ref: SymDenotation =>
      if (isMethodWithExtension(ref) && ref.hasAnnotation(defn.TailrecAnnot)) {
        val ref1 = ref.copySymDenotation()
        ref1.removeAnnotation(defn.TailrecAnnot)
        ref1
      }
      else if (ref.isConstructor && isDerivedValueClass(ref.owner) && ref.is(AccessFlags)) {
        val ref1 = ref.copySymDenotation()
        ref1.resetFlag(AccessFlags)
        ref1
      }
      else ref
    case _ =>
      ref
  }

  protected def rewiredTarget(target: Symbol, derived: Symbol)(implicit ctx: Context): Symbol =
    if (isMethodWithExtension(target) &&
        target.owner.linkedClass == derived.owner) extensionMethod(target)
    else NoSymbol

  private def createExtensionMethod(imeth: Symbol, staticClass: Symbol)(implicit ctx: Context): TermSymbol = {
    val extensionName = extensionNames(imeth).head.toTermName
    val extensionMeth = ctx.newSymbol(staticClass, extensionName,
      (imeth.flags | Final) &~ (Override | Protected | AbsOverride),
      fullyParameterizedType(imeth.info, imeth.owner.asClass),
      privateWithin = imeth.privateWithin, coord = imeth.coord)
    extensionMeth.addAnnotations(imeth.annotations)(ctx.withPhase(thisPhase))
      // need to change phase to add tailrec annotation which gets removed from original method in the same phase.
    extensionMeth
  }

  private val extensionDefs = newMutableSymbolMap[mutable.ListBuffer[Tree]]
  // TODO: this is state and should be per-run
  // todo: check that when transformation finished map is empty

  override def transformTemplate(tree: tpd.Template)(implicit ctx: Context): tpd.Tree = {
    if (isDerivedValueClass(ctx.owner)) {
      /* This is currently redundant since value classes may not
         wrap over other value classes anyway.
        checkNonCyclic(ctx.owner.pos, Set(), ctx.owner) */
      tree
    } else if (ctx.owner.isStaticOwner) {
      extensionDefs remove tree.symbol.owner match {
        case Some(defns) if defns.nonEmpty =>
          cpy.Template(tree)(body = tree.body ++
            defns.map(transformFollowing(_)))
        case _ =>
          tree
      }
    } else tree
  }

  override def transformDefDef(tree: tpd.DefDef)(implicit ctx: Context): tpd.Tree = {
    if (isMethodWithExtension(tree.symbol)) {
      val origMeth = tree.symbol
      val origClass = ctx.owner.asClass
      val staticClass = origClass.linkedClass
      assert(staticClass.exists, s"$origClass lacks companion, ${origClass.owner.definedPeriodsString} ${origClass.owner.info.decls} ${origClass.owner.info.decls}")
      val extensionMeth = extensionMethod(origMeth)
      ctx.log(s"Value class $origClass spawns extension method.\n  Old: ${origMeth.showDcl}\n  New: ${extensionMeth.showDcl}")
      val store: ListBuffer[Tree] = extensionDefs.get(staticClass) match {
        case Some(x) => x
        case None =>
          val newC = new ListBuffer[Tree]()
          extensionDefs(staticClass) = newC
          newC
      }
      store += fullyParameterizedDef(extensionMeth, tree)
      cpy.DefDef(tree)(rhs = forwarder(extensionMeth, tree))
    } else tree
  }
}

object ExtensionMethods {
  /** Generate stream of possible names for the extension version of given instance method `imeth`.
   *  If the method is not overloaded, this stream consists of just "imeth$extension".
   *  If the method is overloaded, the stream has as first element "imeth$extenionX", where X is the
   *  index of imeth in the sequence of overloaded alternatives with the same name. This choice will
   *  always be picked as the name of the generated extension method.
   *  After this first choice, all other possible indices in the range of 0 until the number
   *  of overloaded alternatives are returned. The secondary choices are used to find a matching method
   *  in `extensionMethod` if the first name has the wrong type. We thereby gain a level of insensitivity
   *  of how overloaded types are ordered between phases and picklings.
   */
  private def extensionNames(imeth: Symbol)(implicit ctx: Context): Stream[Name] = {
    val decl = imeth.owner.info.decl(imeth.name)

    /** No longer needed for Dotty, as we are more disciplined with scopes now.
    // Bridge generation is done at phase `erasure`, but new scopes are only generated
    // for the phase after that. So bridges are visible in earlier phases.
    //
    // `info.member(imeth.name)` filters these out, but we need to use `decl`
    // to restrict ourselves to members defined in the current class, so we
    // must do the filtering here.
    val declTypeNoBridge = decl.filter(sym => !sym.isBridge).tpe
    */
    decl match {
      case decl: MultiDenotation =>
        val alts = decl.alternatives
        val index = alts indexOf imeth.denot
        assert(index >= 0, alts + " does not contain " + imeth)
        def altName(index: Int) = UniqueExtMethName(imeth.name.asTermName, index)
        altName(index) #:: ((0 until alts.length).toStream filter (index != _) map altName)
      case decl =>
        assert(decl.exists, imeth.name + " not found in " + imeth.owner + "'s decls: " + imeth.owner.info.decls)
        Stream(ExtMethName(imeth.name.asTermName))
    }
  }

  /** Return the extension method that corresponds to given instance method `meth`. */
  def extensionMethod(imeth: Symbol)(implicit ctx: Context): TermSymbol =
    ctx.atPhase(ctx.extensionMethodsPhase.next) { implicit ctx =>
      // FIXME use toStatic instead?
      val companionInfo = imeth.owner.companionModule.info
      val candidates = extensionNames(imeth) map (companionInfo.decl(_).symbol) filter (_.exists)
      val matching = candidates filter (c => FullParameterization.memberSignature(c.info) == imeth.signature)
      assert(matching.nonEmpty,
       i"""no extension method found for:
          |
          |  $imeth:${imeth.info.show} with signature ${imeth.signature}
          |
          | Candidates:
          |
          | ${candidates.map(c => c.name + ":" + c.info.show).mkString("\n")}
          |
          | Candidates (signatures normalized):
          |
          | ${candidates.map(c => c.name + ":" + c.info.signature + ":" + FullParameterization.memberSignature(c.info)).mkString("\n")}
          |
          | Eligible Names: ${extensionNames(imeth).mkString(",")}""")
      matching.head.asTerm
    }
}
