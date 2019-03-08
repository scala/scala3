package dotty.tools.dotc.tastyreflect

trait ContextOpsImpl extends scala.tasty.reflect.ContextOps with CoreImpl {

  val rootContext: Context

  def ContextDeco(ctx: Context): ContextAPI = new ContextAPI {
    def owner: Symbol = ctx.owner

    def source: java.nio.file.Path = ctx.compilationUnit.source.file.jpath
  }

}
