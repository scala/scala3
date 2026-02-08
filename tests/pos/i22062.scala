class Label
class Component

trait RenderableCellsCompanion:
  type Renderer[-A] <: CellRenderer[A]
  type DefaultRenderer[-A] <: Label & Renderer[A]

  trait CellRendererCompanion:
    type CellInfo
    def labeled[A](): DefaultRenderer[A]
    protected trait LabelRenderer[-A] extends CellRenderer[A]:
      override abstract def componentFor(info: companion.CellInfo): Component = super.componentFor(info)

  trait CellRenderer[-A]:
    val companion: CellRendererCompanion
    def componentFor(cellInfo: companion.CellInfo): Component

sealed trait TreeRenderers extends RenderableCellsCompanion:
  this: Tree.type =>

  trait Renderer[-A] extends CellRenderer[A]:
    final override val companion = Renderer

  object Renderer extends CellRendererCompanion:
    final class CellInfo
    override def labeled[A]() = new DefaultRenderer[A] with LabelRenderer[A] {}

  class DefaultRenderer[-A] extends Label with Renderer[A]:
    override def componentFor(info: Renderer.CellInfo): Component = ???

class Tree extends Component
object Tree extends TreeRenderers
