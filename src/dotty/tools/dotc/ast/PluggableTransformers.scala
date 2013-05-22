package dotty.tools.dotc
package ast


object PluggableTransformers {
/*
  import Trees._, Contexts._

  abstract class PluggableTransformer[T] extends TreeTransformer[T, Context] {
    type PluginOp[-N <: Tree[T]] = N => Tree[T]

    private[this] var _ctx: Context = _
    private[this] var _oldTree: Tree[T] = _

    protected implicit def ctx: Context = _ctx
    protected def oldTree: Tree[T] = _oldTree
    protected def thisTransformer: PluggableTransformer[T] = this

    class PluginOps[-N <: Tree[T]](op: PluginOp[N], val next: Plugins) {
      def apply(tree: N, old: Tree[T], c: Context): Tree[T] = {
        val savedCtx = _ctx
        val savedOld = _oldTree
        try {
          op(tree)
        } finally {
          _oldTree = savedOld
          _ctx = savedCtx
        }
      }
    }

    val NoOp: PluginOp[Tree[T]] = identity
    val NoOps = new PluginOps(NoOp, null)

    class Plugins {
      def next: Plugins = null

      def processIdent: PluginOp[Ident[T]] = NoOp
      def processSelect: PluginOp[Select[T]] = NoOp

      val IdentOps: PluginOps[Ident[T]] = NoOps
      val SelectOps: PluginOps[Select[T]] = NoOps
    }

    val EmptyPlugin = new Plugins

    private[this] var _plugins: Plugins = EmptyPlugin

    override def plugins: Plugins = _plugins

    class Plugin extends Plugins {
      override val next = _plugins
      _plugins = this

      private def push[N <: Tree[T]](op: PluginOp[N], ops: => PluginOps[N]): PluginOps[N] =
        if (op == NoOp) ops else new PluginOps(op, next)

      override val IdentOps: PluginOps[Ident[T]] = push(processIdent, next.IdentOps)
      override val SelectOps: PluginOps[Select[T]] = push(processSelect, next.SelectOps)
    }

    def postIdent(tree: Ident[T], old: Tree[T], c: Context, ops: PluginOps[Ident[T]]) =
      if (ops eq NoOps) tree
      else finishIdent(ops(tree, old, c), old, c, ops.next)

    override def finishIdent(tree: Tree[T], old: Tree[T], c: Context, plugins: Plugins): Tree[T] = tree match {
      case tree: Ident[_] => postIdent(tree, old, c, plugins.IdentOps)
      case _ => postProcess(tree, old, c, plugins)
    }

    def postSelect(tree: Select[T], old: Tree[T], c: Context, ops: PluginOps[Select[T]]) =
      if (ops eq NoOps) tree
      else finishSelect(ops(tree, old, c), old, c, ops.next)

    override def finishSelect(tree: Tree[T], old: Tree[T], c: Context, plugins: Plugins): Tree[T] = tree match {
      case tree: Select[_] => postSelect(tree, old, c, plugins.SelectOps)
      case _ => postProcess(tree, old, c, plugins)
    }

    protected def postProcess(tree: Tree[T], old: Tree[T], c: Context, plugins: Plugins): Tree[T] = tree match {
      case tree: Ident[_] => finishIdent(tree, old, c, plugins)
      case tree: Select[_] => finishSelect(tree, old, c, plugins)
    }
  }
}

import PluggableTransformers._, Types._, Trees._, Contexts._

class ExampleTransformer extends PluggableTransformer[Type] {

  object ExamplePlugin extends Plugin {
    override def processIdent = {
      case tree @ Ident(x) if x.isTypeName => tree.derivedSelect(tree, x)
      case tree => tpd.Ident(???)
    }
    override def processSelect = { tree =>
      if (tree.isType) tree.derivedIdent(tree.name)
      else tpd.EmptyTree
    }
  }

  override def transform(tree: tpd.Tree, ctx: Context) =
    super.transform(tree, ctx)
*/
}