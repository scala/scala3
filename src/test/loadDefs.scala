package test

import dotty.tools.dotc.core._
import Contexts._

object loadDefs {

  def main(args: Array[String]) = {
    val base = new ContextBase
    implicit val ctx = base.initialCtx
    println(ctx.settings)
    base.definitions.init()
    println("done")
  }
}