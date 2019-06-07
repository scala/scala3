package scala.tasty
package reflect

trait IdOps extends Core {

  implicit class IdAPI(id: Id) {

    /** Position in the source code */
    def pos(implicit ctx: Context): Position = kernel.Id_pos(id)

    /** Name of the identifier */
    def name(implicit ctx: Context): String = kernel.Id_name(id)

  }

  object Id {
    def unapply(id: Id)(implicit ctx: Context): Option[String] = Some(id.name)
  }

}
