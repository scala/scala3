/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */
package dotty.tools.dotc
package transform

import dotty.tools.dotc.transform.TreeTransforms.{TransformerInfo, TreeTransform, TreeTransformer}
import dotty.tools.dotc.ast.{Trees, tpd}
import scala.collection.{ mutable, immutable }
import mutable.ListBuffer
import scala.annotation.tailrec
import core._
import Types._, Contexts._, Constants._, Names._, NameOps._, Flags._, DenotTransformers._
import SymDenotations._, Symbols._, StdNames._, Annotations._, Trees._, Scopes._, Denotations._
import TypeUtils._
import util.Positions._
import Decorators._

/**
 * Perform Step 1 in the inline classes SIP: Creates extension methods for all
 * methods in a value class, except parameter or super accessors, or constructors.
 */
class ExtensionMethods extends MacroTransform with IdentityDenotTransformer { thisTransformer =>

  import tpd._

  /** the following two members override abstract members in Transform */
  val name: String = "extmethods"

  def newTransformer(implicit ctx: Context): Transformer = new Extender

  /** Generate stream of possible names for the extension version of given instance method `imeth`.
   *  If the method is not overloaded, this stream consists of just "extension$imeth".
   *  If the method is overloaded, the stream has as first element "extensionX$imeth", where X is the
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
      case decl: SingleDenotation =>
        val alts = decl.alternatives
        val index = alts indexOf imeth.denot
        assert(index >= 0, alts+" does not contain "+imeth)
        def altName(index: Int) = (imeth.name+"$extension"+index).toTermName
        altName(index) #:: ((0 until alts.length).toStream filter (index != _) map altName)
      case tpe =>
        assert(tpe != NoType, imeth.name+" not found in "+imeth.owner+"'s decls: "+imeth.owner.info.decls)
        Stream((imeth.name+"$extension").toTermName)
    }
  }

  /** Return the extension method that corresponds to given instance method `meth`. */
  def extensionMethod(imeth: Symbol)(implicit ctx: Context): Symbol =
    ctx.atPhase(thisTransformer.next) { implicit ctx =>
      // FIXME use toStatic instead?
      val companionInfo = imeth.owner.companionModule.info
      val candidates = extensionNames(imeth) map (companionInfo.decl(_).symbol) filter (_.exists)
      val matching = candidates filter (alt => alt.info.toDynamic(imeth.owner) matches imeth.info)
      assert(matching.nonEmpty,
        sm"""|no extension method found for:
             |
             |  $imeth:${imeth.info}
             |
             | Candidates:
             |
             | ${candidates.map(c => c.name + ":" + c.info).mkString("\n")}
             |
             | Candidates (signatures normalized):
             |
             | ${candidates.map(c => c.name + ":" + c.info.toDynamic(imeth.owner)).mkString("\n")}
             |
             | Eligible Names: ${extensionNames(imeth).mkString(",")}""")
      matching.head
    }

  class Extender extends Transformer {
    private val extensionDefs = mutable.Map[Symbol, mutable.ListBuffer[Tree]]()

    def checkNonCyclic(pos: Position, seen: Set[Symbol], clazz: ClassSymbol)(implicit ctx: Context): Unit =
      if (seen contains clazz)
        ctx.error("value class may not unbox to itself", pos)
      else {
        val unboxed = clazz.underlyingOfValueClass.typeSymbol
        if (unboxed.isDerivedValueClass) checkNonCyclic(pos, seen + clazz, unboxed.asClass)
      }

    override def transform(tree: Tree)(implicit ctx: Context): Tree = {
      tree match {
        case tree: Template =>
          if (ctx.owner.isDerivedValueClass) {
          /* This is currently redundant since value classes may not
             wrap over other value classes anyway.
            checkNonCyclic(ctx.owner.pos, Set(), ctx.owner) */
            extensionDefs(ctx.owner.companionModule) = new mutable.ListBuffer[Tree]
            ctx.owner.primaryConstructor.makeNotPrivateAfter(NoSymbol, thisTransformer)
            // SI-7859 make param accessors accessible so the erasure can generate unbox operations.
            val paramAccessors = ctx.owner.info.decls.filter(_.is(ParamAccessor))
            paramAccessors.foreach(_.makeNotPrivateAfter(ctx.owner, thisTransformer))
            super.transform(tree)
          } else if (ctx.owner.isStaticOwner) {
            val tree1 @ Template(constr, parents, selfType, body) = super.transform(tree)
            extensionDefs remove tree1.symbol.owner match {
              case Some(defns) if defns.nonEmpty =>
                cpy.Template(tree1, constr, parents, selfType, body ++ defns)
              case _ =>
                tree1
            }
          } else tree
        case DefDef(mods, name, tparams, vparamss, tpt, rhs) if tree.symbol.isMethodWithExtension =>
          val origMeth      = tree.symbol
          val origClass     = ctx.owner.asClass
          val origTParams   = tparams.map(_.symbol) ::: origClass.typeParams   // method type params ++ class type params
          val origVParams   = vparamss.flatten map (_.symbol)
          val staticClass   = origClass.companionClass
          assert(staticClass.exists)

          val extensionMeth = ctx.atPhase(thisTransformer.next) { implicit ctx =>
            val extensionName = extensionNames(origMeth).head.toTermName
            val extensionMeth = ctx.newSymbol(staticClass, extensionName,
                  origMeth.flags | Final &~ (Override | Protected | AbsOverride),
                  origMeth.info.toStatic(origClass),
                  privateWithin = origMeth.privateWithin, coord = tree.pos)
            extensionMeth.addAnnotations(from = origMeth)
            origMeth.removeAnnotation(defn.TailrecAnnotationClass) // it's on the extension method, now.
            extensionMeth.enteredAfter(thisTransformer)
          }
          ctx.log(s"Value class $origClass spawns extension method.\n  Old: ${origMeth.showDcl}\n  New: ${extensionMeth.showDcl}")

          extensionDefs(staticClass) += polyDefDef(extensionMeth, trefs => vrefss => {
            def methPart(tp: Type): MethodType = tp match {
              case tp: PolyType => methPart(tp.resultType)
              case tp: MethodType => tp
            }
            val substitutions: Type => Type = _
              .subst(origTParams, trefs)
              .substSym(origVParams, vrefss.flatten.map(_.symbol))
              .substThis(origClass, MethodParam(methPart(extensionMeth.info), 0))
            new TreeTypeMap(substitutions, Map(origMeth -> extensionMeth)).transform(rhs)
          })

          // These three lines are assembling Foo.bar$extension[T1, T2, ...]($this)
          // which leaves the actual argument application for extensionCall.
          val forwarder = ref(extensionMeth.termRef)
            .appliedToTypes(origTParams.map(_.typeRef))
            .appliedTo(This(origClass))
            .appliedToArgss(vparamss.nestedMap(vparam => ref(vparam.symbol)))
            .withPos(rhs.pos)
          cpy.DefDef(tree, mods, name, tparams, vparamss, tpt, forwarder)
        case _ =>
          super.transform(tree)
      }
    }
  }
}
