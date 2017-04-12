package dotty.tools
package vulpix

sealed trait Status
final case class Success(output: String) extends Status
final case class Failure(output: String) extends Status
final case object Timeout extends Status
