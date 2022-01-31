package dotty.tools.dotc
package transform

import core._
import Contexts.*
import DenotTransformers.IdentityDenotTransformer
import Decorators.*
import SyntheticMembers.*
import ast.tpd.*

/** A phase that adds mirror support for anonymous mirrors created at inlining. */
class PostInlining extends MacroTransform, IdentityDenotTransformer:
  thisPhase =>

  override def phaseName: String = PostInlining.name

  override def description: String = PostInlining.description

  override def changesMembers = true

  override def run(using Context): Unit =
    if ctx.compilationUnit.needsMirrorSupport then super.run

  lazy val synthMbr: SyntheticMembers = new SyntheticMembers(thisPhase)

  def newTransformer(using Context): Transformer = new Transformer:
    override def transform(tree: Tree)(using Context): Tree =
      super.transform(tree) match
        case tree1: Template
        if tree1.hasAttachment(ExtendsSingletonMirror)
          || tree1.hasAttachment(ExtendsProductMirror)
          || tree1.hasAttachment(ExtendsSumMirror) =>
          synthMbr.addMirrorSupport(tree1)
        case tree1 => tree1

object PostInlining:
  val name: String = "postInlining"
  val description: String = "add mirror support for inlined code"
