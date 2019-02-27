package dotty.tools.dotc.tastyreflect

import dotty.tools.dotc.core.Decorators._

trait IdOpsImpl extends scala.tasty.reflect.IdOps with CoreImpl {

  object Id extends IdModule {
    def unapply(id: Id): Option[String] = Some(id.name.toString)
  }

}
