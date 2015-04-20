package dotty.tools.dotc
package typer

import core._
import Contexts.Context
import Decorators._
import Phases._
import Types._, Symbols._, Flags._, StdNames._
import util.Positions._
import ast.Trees._
import typer.ErrorReporting._
import DenotTransformers._

/** This checks `New` nodes, verifying that they can be instantiated. */
class InstChecks extends Phase with IdentityDenotTransformer {
  import ast.tpd._

  override def phaseName: String = "instchecks"

  override def run(implicit ctx: Context): Unit =
    instCheck.traverse(ctx.compilationUnit.tpdTree)
  
  /** Check that `tp` refers to a nonAbstract class
   *  and that the instance conforms to the self type of the created class.
   */
  def checkInstantiatable(tp: Type, pos: Position)(implicit ctx: Context): Unit =
    tp.underlyingClassRef(refinementOK = false) match {
      case tref: TypeRef =>
        val cls = tref.symbol
        if (cls.is(AbstractOrTrait))
          ctx.error(d"$cls is abstract; cannot be instantiated", pos)
        if (!cls.is(Module)) {
          val selfType = tp.givenSelfType.asSeenFrom(tref.prefix, cls.owner)
          if (selfType.exists && !(tp <:< selfType))
            ctx.error(d"$tp does not conform to its self type $selfType; cannot be instantiated")
        }
      case _ =>
    }
  
  def checkValidJavaAnnotation(annot: Tree)(implicit ctx: Context): Unit = {
    // TODO fill in
  }

  val instCheck = new TreeTraverser {
    
    def checkAnnot(annot: Tree)(implicit ctx: Context): Unit = 
      if (annot.symbol.is(JavaDefined)) checkValidJavaAnnotation(annot)
      else traverse(annot)
      
    def traverseNoCheck(tree: Tree)(implicit ctx: Context): Unit = tree match {
      case Apply(fn, args) =>
        traverseNoCheck(fn)
        args.foreach(traverse)
      case TypeApply(fn, args) =>
        traverseNoCheck(fn)
        args.foreach(traverse)
      case Select(qual, nme.CONSTRUCTOR) =>
        traverseNoCheck(qual)
      case New(tpt) =>
        traverse(tpt)
      case _ =>
        traverse(tree)
    }
    
    def traverse(tree: Tree)(implicit ctx: Context): Unit = tree match {
      case tree: New =>
        checkInstantiatable(tree.tpe, tree.pos)
        traverseChildren(tree)
      case impl @ Template(constr, parents, self, _) =>
        traverse(constr)
        parents.foreach(traverseNoCheck)
        traverse(self)
        impl.body.foreach(traverse)
      case Annotated(annot, tree) =>
        checkAnnot(annot)
        traverse(tree)
      case TypeTree(original) =>
        tree.tpe match {
          case AnnotatedType(annot, tpe) => checkAnnot(annot.tree)
          case _ =>
        }
        traverse(original)
      case tree: MemberDef =>
        tree.symbol.annotations.foreach(annot => checkAnnot(annot.tree))
        traverseChildren(tree)
      case _ =>
        traverseChildren(tree)
    }
  }
}
