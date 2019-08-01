package scala.tasty
package reflect

trait IdOps extends Core {

  implicit class IdAPI(id: Id) {

    /** Position in the source code */
    def pos given (ctx: Context): Position = internal.Id_pos(id)

    /** Name of the identifier */
    def name given (ctx: Context): String = internal.Id_name(id)

  }

  object Id {
    def unapply(id: Id) given (ctx: Context): Option[String] = Some(id.name)
  }

}
