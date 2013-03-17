package test

import dotty.tools.dotc.core._
import Contexts._
import Symbols._, Types._
import Decorators._

object showClass {

  def showClass(path: String)(implicit ctx: Context): Unit = {
    println(s"showing $path")
    val cls = ctx.requiredClass(path.toTypeName)
    println(s"showing $path -> ${cls.denot}")
    val info = cls.info
    info match {
      case ClassInfo(pre, c, cps, decls, optSelfType) =>
        println(s"prefix = ${pre.show}")
        println(s"self = ${c.show}")
        println(s"parents = ${cps.map(_.show).mkString(",")}")
        println(s"decls = ${decls.show}")
        println(s"selftype = ${optSelfType.show}")
    }
  }

  def main(args: Array[String]) = {
    val base = Context.theBase
    implicit val ctx = base.initialCtx
    println(ctx.settings)
    base.definitions.init()

    for (arg <- args) showClass(arg)

    showClass("java.lang.Class")
    showClass("scala.Boolean")
    showClass("scala.Array")
    showClass("scala.math.Ordering")
    showClass("scala.collection.Traversable")
    showClass("scala.collection.LinearSeqLike")
    showClass("scala.collection.immutable.List")
    showClass("scala.collection.TraversableLike")
    showClass("scala.collection.immutable.Seq")
    showClass("scala.collection.MapLike")
    //showClass("dotty.tools.dotc.core.Types")
    println("done")
  }
}