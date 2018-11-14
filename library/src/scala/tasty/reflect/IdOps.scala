package scala.tasty
package reflect

trait IdOps extends Core {

  trait IdAPI {
    /** Position in the source code */
    def pos(implicit ctx: Context): Position
    def name(implicit ctx: Context): String
  }
  implicit def IdDeco(id: Id): IdAPI

  val Id: IdExtractor
  abstract class IdExtractor {
    def unapply(id: Id): Option[String]
  }

}
