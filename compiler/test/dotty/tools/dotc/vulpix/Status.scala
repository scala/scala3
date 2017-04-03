package dotty
package tools
package dotc
package vulpix

/** The status of each call to `main` in the test applications */
sealed trait Status extends Serializable
final case class Success(output: String) extends Status
final case class Failure(output: String) extends Status
final case object Timeout extends Status
