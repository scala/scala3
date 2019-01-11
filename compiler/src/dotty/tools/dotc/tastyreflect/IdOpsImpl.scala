package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.Decorators._

trait IdOpsImpl extends scala.tasty.reflect.IdOps with CoreImpl {

  def IdDeco(id: Id): IdAPI = new IdAPI {
    def pos(implicit ctx: Context): Position = id.pos
    def name(implicit ctx: Context): String = id.name.toString
  }

  object Id extends IdModule {
    def unapply(id: Id): Option[String] = Some(id.name.toString)
  }

}
