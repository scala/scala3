package scala.tasty.reflect

trait CommentOps extends Core {

  implicit class CommentAPI(self: Comment) {

    /** Raw comment string */
    def raw: String = kernel.Comment_raw(self)

    /** Expanded comment string, if any */
    def expanded: Option[String] = kernel.Comment_expanded(self)

    /** List of usecases and their corresponding trees, if any */
    def usecases: List[(String, Option[DefDef])] = kernel.Comment_usecases(self)

  }

}
