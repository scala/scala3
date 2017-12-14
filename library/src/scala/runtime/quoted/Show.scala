package scala.runtime.quoted

import scala.annotation.implicitNotFound
import scala.quoted.Expr

@implicitNotFound("Could not find implicit Show. Default runner can must be imported with `import dotty.tools.dotc.quoted.Runners._`")
trait Show[T] {
  def run(expr: Expr[T]): String
}
