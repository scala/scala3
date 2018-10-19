package xsbt

import java.util.Optional
import xsbti.{Position, Severity}

final case class Problem(override val position: Position,
                         override val message: String,
                         override val severity: Severity,
                         rendered0: String) extends xsbti.Problem {
  override val category = ""
  override val rendered = Optional.of(rendered0)
}
