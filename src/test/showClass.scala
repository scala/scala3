package test

import dotty.tools.dotc.core._
import Contexts._
import Symbols._, Types._, dotty.tools.dotc.util.Texts._
import Decorators._

object showClass {

  def showClasses(path: String)(implicit ctx: Context): Unit = {
    def showClass(cls: Symbol) = {
      println(s"showing $path -> ${cls.denot}")
      val cinfo = cls.info
      val infoText: Text = if (cinfo.exists) cinfo.toText else " is missing"
      println("======================================")
      println((cls.toText ~ infoText).show)
    }
    println(s"showing $path")
    val cls = ctx.requiredClass(path.toTypeName)
    showClass(cls)
    showClass(cls.linkedClass)
/*
    val info = cls.info
    info match {
      case ClassInfo(pre, c, cps, decls, optSelfType) =>
        println(s"prefix = ${pre.show}")
        println(s"self = ${c.show}")
        println(s"parents = ${cps.map(_.show).mkString(",")}")
        println(s"showClass $path") // !!! DEBUG
        println(s"decls = ${decls.show}")
        println(s"selftype = ${optSelfType.show}")
        println(s"type-params = ${info.typeParams}")
    }
*/
  }

  def main(args: Array[String]) = {
    val base = Context.theBase
    implicit val ctx = base.initialCtx
    println(ctx.settings)
    base.definitions.init()

    for (arg <- args) showClasses(arg)

//    showClasses("java.util.Map")
//    showClasses("scala.Boolean")
//    showClasses("scala.Array")
//    showClasses("scala.math.Ordering")
      showClasses("scala.collection.mutable.LinkedHashMap")
//    showClasses("scala.collection.Traversable")
//    showClasses("scala.collection.LinearSeqLike")
//    showClasses("scala.collection.immutable.List")
//    showClasses("scala.collection.convert.Wrappers")
//    showClasses("scala.collection.generic.package")
//    showClasses("scala.collection.MapLike")
//    showClasses("scala.Function1")
//  showClasses("dotty.tools.dotc.core.Types")
    println("done")
  }
}