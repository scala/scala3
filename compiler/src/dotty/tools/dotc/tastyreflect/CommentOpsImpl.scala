package dotty.tools.dotc.tastyreflect

trait CommentOpsImpl extends scala.tasty.reflect.CommentOps with CoreImpl {

  implicit def CommentDeco(com: Comment): CommentAPI = new CommentAPI {
    override def raw: String = com.raw
    override def expanded: Option[String] = com.expanded
    override def usecases: List[(String, Option[DefDef])] = com.usecases.map { uc => (uc.code, uc.tpdCode) }
  }
}
