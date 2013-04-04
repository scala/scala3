package test

import dotty.tools.dotc.core._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Types._
import dotty.tools.dotc.util.Texts._
import dotty.tools.dotc.core.Decorators._
import org.junit.Test

class ShowClassTests {

  private val blackList = List(
    // the following classes cannot be read correctly because they
    // contain illegally pickled @throws annotations
    "scala.actors.remote.Proxy",
    "scala.actors.remote.Serializer",
    "scala.actors.remote.JavaSerializer",
    "scala.build.genprod",
    "scala.tools.nsc.symtab.classfile.AbstractFileReader",
    "scala.remoting.Channel",
    "scala.runtime.remoting.RegistryDelegate",
    "scala.concurrent.Future",
    "scala.concurrent.impl.Future",
    "scala.concurrent.Await",
    "scala.concurrent.Awaitable",
    "scala.concurrent.impl.Promise",
    // the following packages and classes cannot be read because
    // theyt refer to external libraries which are not available
    // (apache.ant, usually)
    "scala.tools.ant",
    "scala.tools.partest.PartestTask")

  def showPackage(pkg: TermSymbol)(implicit ctx: Context): Unit = {
    val path = pkg.fullName.toString
    if (blackList contains path)
      println(s"blacklisted package: $path")
    else {
      for (
        sym <- pkg.info.decls if sym.owner == pkg.moduleClass && !(sym.name contains '$')
      ) {
        println(s"showing $sym in ${pkg.fullName}")
        if (sym is Package) showPackage(sym.asTerm)
        else if (sym.isClass) showClass(sym)
        else if (sym is Module) showClass(sym.moduleClass)
      }
    }
  }

  def showPackage(path: String)(implicit ctx: Context): Unit =
    showPackage(ctx.requiredPackage(path))

  def showClass(cls: Symbol)(implicit ctx: Context) = {
    val path = cls.fullName.toString
    if (blackList contains path)
      println(s"blacklisted: $path")
    else {
      println(s"showing $path -> ${cls.denot}")
      val cinfo = cls.info
      val infoText: Text = if (cinfo.exists) cinfo.toText else " is missing"
      println("======================================")
      println((cls.toText ~ infoText).show)
    }
  }

  def showClasses(path: String)(implicit ctx: Context): Unit = {
    println(s"showing file $path")
    val cls = ctx.requiredClass(path.toTypeName)
    showClass(cls)
    showClass(cls.linkedClass)
  }

  implicit val ctx: Context = {
    val base = Context.theBase
    import base.settings._
    val ctx = base.initialCtx.fresh
      .withSetting(verbose, true)
      .withSetting(debug, true)
      //    .withSetting(settings.debugNames, true)
      .withSetting(Ylogcp, true)
      .withSetting(printtypes, true)
      .withSetting(pageWidth, 90)
      .withSetting(log, List("<some"))
    println(ctx.settings)
    base.definitions.init()
    ctx
  }

  @Test
  def loadSimpleClasses() = {
    showClasses("scala.Array")
    showClasses("scala.math.Ordering")
  }

  @Test
  def loadMoreClasses() = {
    showClasses("scala.collection.JavaConversions")
    showClasses("scala.collection.convert.Wrappers")
    showClasses("scala.collection.mutable.WeakHashMap")
    showClasses("scala.collection.GenIterable")
    showClasses("scala.collection.Traversable")
    showClasses("scala.collection.LinearSeqLike")
    showClasses("scala.collection.immutable.List")
    showClasses("scala.collection.convert.Wrappers")
    showClasses("scala.collection.generic.package")
    showClasses("scala.collection.MapLike")
    showClasses("scala.Function1")
  }

  @Test
  def loadScalaReflect() = {
    showPackage(ctx.requiredPackage("scala.reflect"))
  }

  @Test
  def loadClassWithPrivateInnerAndSubSelf() = {
    showClasses("scala.tools.nsc.settings.ScalaSettings")
    showClasses("scala.tools.jline.console.history.MemoryHistory")
  }

  @Test
  def loadJlineHistory() = {
    showPackage("scala.tools.jline.console.history")
  }

  @Test
  def showReflectAliases() = { // tests for cycles during findMember
    showClasses("scala.reflect.macros.runtime.Aliases")
  }
}
