package dotty.tools.dotc.tastyreflect

trait ContextOpsImpl extends scala.tasty.reflect.ContextOps with CoreImpl {

  // overridden to remove the implicit modifier inside the implementation
  override def rootContext: Context = super.rootContext

}
