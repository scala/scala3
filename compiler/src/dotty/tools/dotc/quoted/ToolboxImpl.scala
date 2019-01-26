/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package dotty.tools.dotc.quoted

import dotty.tools.dotc.ast.tpd

import scala.quoted.Expr
import scala.quoted.Exprs.{LiftedExpr, TastyTreeExpr}

/** Default runners for quoted expressions */
object ToolboxImpl {
  import tpd._

  def make(settings: scala.quoted.Toolbox.Settings): scala.quoted.Toolbox = new scala.quoted.Toolbox {

    private[this] val driver: QuoteDriver = new QuoteDriver()

    def run[T](expr: Expr[T]): T = expr match {
      case expr: LiftedExpr[T] =>
        expr.value
      case expr: TastyTreeExpr[Tree] @unchecked =>
        throw new Exception("Cannot call `Expr.run` on an `Expr` that comes from a macro argument.")
      case _ =>
        synchronized(driver.run(expr, settings))
    }

    def show[T](expr: Expr[T]): String = synchronized(driver.show(expr, settings))

  }
}
