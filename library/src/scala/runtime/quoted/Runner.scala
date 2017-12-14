package scala.runtime.quoted

import scala.annotation.implicitNotFound
import scala.quoted.Expr

@implicitNotFound("Could not find implicit Runner. Default runner can must be imported with `import dotty.tools.dotc.quoted.Runners._`")
trait Runner[T] {
  def run(expr: Expr[T]): T
}
