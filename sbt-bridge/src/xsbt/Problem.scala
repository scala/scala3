package xsbt

import xsbti.{Position, Severity}

final case class Problem(override val position: Position,
                         override val message: String,
                         override val severity: Severity) extends xsbti.Problem {
  override val category = ""
  override def toString = s"[$severity] $position: $message"

}

