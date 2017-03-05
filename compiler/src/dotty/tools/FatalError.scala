package dotty.tools

case class FatalError(msg: String) extends Exception(msg)
