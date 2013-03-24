package test

import dotty.tools.dotc.core._
import Contexts._
import Symbols._, Flags._, Types._, dotty.tools.dotc.util.Texts._
import Decorators._

object showClass {

  def showPackage(pkg: TermSymbol)(implicit ctx: Context) {
    for (sym <- pkg.info.decls
         if sym.owner == pkg.moduleClass && !(sym.name contains '$')) {
      println(s"showing $sym in ${pkg.fullName}")
      if (sym is Package) showPackage(sym.asTerm)
      else if (sym.isClass) showClass(sym)
      else showClass(sym.moduleClass)
    }
  }

  def showClass(cls: Symbol)(implicit ctx: Context) = {
    println(s"showing ${cls.denot}")
    val cinfo = cls.info
    val infoText: Text = if (cinfo.exists) cinfo.toText else " is missing"
    println("======================================")
    println((cls.toText ~ infoText).show)
  }

  def showClasses(path: String)(implicit ctx: Context): Unit = {
    println(s"showing file $path")
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

    for (arg <- args) showPackage(ctx.requiredPackage(arg))

      showClasses("scala.actors.remote.Proxy")
//    showClasses("scala.Boolean")
//    showClasses("scala.Array")
//    showClasses("scala.math.Ordering")
//      showClasses("scala.collection.JavaConversions")
//      showClasses("scala.collection.convert.Wrappers")
//      showClasses("scala.collection.mutable.WeakHashMap")
//      showClasses("scala.collection.GenIterable")
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