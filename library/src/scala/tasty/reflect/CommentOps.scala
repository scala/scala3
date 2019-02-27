package scala.tasty.reflect

trait CommentOps extends Core {

  trait CommentAPI {

    /** Raw comment string */
    def raw: String

    /** Expanded comment string, if any */
    def expanded: Option[String]

    /** List of usecases and their corresponding trees, if any */
    def usecases: List[(String, Option[DefDef])]

  }
  implicit def CommentDeco(comment: Comment): CommentAPI

}
