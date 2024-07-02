// minimised from github.com/Adam-Vandervorst/CZ2

import scala.collection.mutable

private trait EMImpl[V, F[_]]

case class EM[V2](apps: ExprMap[ExprMap[V2]]) extends EMImpl[V2, EM]:
  def collect[W](pf: PartialFunction[V2, W]): Unit =
    val apps1 = apps.collect(_.collect(pf))

case class ExprMap[V](var em: EM[V] = null) extends EMImpl[V, ExprMap]:
  def collect[W](pf: PartialFunction[V, W]): ExprMap[W] = ??? // was: StackOverflow in isCheckable
