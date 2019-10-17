package scala.tasty.reflect

trait CommentOps extends Core {

  given (self: Comment) {

    /** Raw comment string */
    def raw: String = internal.Comment_raw(self)

    /** Expanded comment string, if any */
    def expanded: Option[String] = internal.Comment_expanded(self)

    /** List of usecases and their corresponding trees, if any */
    def usecases: List[(String, Option[DefDef])] = internal.Comment_usecases(self)

  }

}
