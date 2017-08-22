package dotty
package tools

import dotc.core._
import dotc.core.Contexts._
import dotc.core.Symbols._
import dotc.core.Flags._
import dotc.core.Types._
import dotc.printing.Texts._
import NameOps._
import dotc.core.Decorators._
import org.junit.Test

class ShowClassTests extends DottyTest {
  ctx = {
    val base = new ContextBase
    import base.settings._
    val ctx = base.initialCtx.fresh
    ctx.setSetting(ctx.settings.encoding, "UTF8")
    ctx.setSetting(
      ctx.settings.classpath,
      Jars.dottyLib + ":" + Jars.dottyInterfaces
    )
    base.initialize()(ctx)
    ctx
  }

  def debug_println(msg: => Any) = {
    if (sys.props.isDefinedAt("test.ShowClassTests.verbose"))
      println(msg)
  }

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
    // the following class cannot be read because it does not exist anymore
    "scala.reflect.macros.Context",
    // the following packages and classes cannot be read because
    // they refer to external libraries which are not available
    // (apache.ant, usually)
    "scala.tools.ant",
    "scala.tools.partest.PartestTask",
    "dotty.tools.dotc.core.pickling.AbstractFileReader")

  def doTwice(test: Context => Unit)(implicit ctx: Context): Unit = {
    test(ctx.fresh.setSetting(ctx.base.settings.debug, true))
    test(ctx.fresh.setSetting(ctx.base.settings.debug, false))
  }

  def showPackage(pkg: TermSymbol)(implicit ctx: Context): Unit = {
    val path = pkg.fullName.toString
    if (blackList contains path)
      debug_println(s"blacklisted package: $path")
    else {
      pkg.info.decls
        .filter(sym => sym.owner == pkg.moduleClass && !(sym.name.toString contains '$'))
        .foreach { sym =>
          debug_println(s"showing $sym in ${pkg.fullName}")
          if (sym is PackageVal) showPackage(sym.asTerm)
          else if (sym.isClass && !(sym is Module)) showClass(sym)
          else if (sym is ModuleVal) showClass(sym.moduleClass)
        }
    }
  }

  def showPackage(path: String, expectedStubs: Int)(implicit ctx: Context): Unit = doTwice { implicit ctx =>
    showPackage(ctx.requiredPackage(path))
    val nstubs = Symbols.stubs.length
    debug_println(s"$nstubs stubs")
    assert(nstubs <= expectedStubs, s"stubs found: $nstubs, expected: $expectedStubs\nstubs: ${Symbols.stubs.mkString(",")}")
  }

  def showClass(cls: Symbol)(implicit ctx: Context) = {
    val path = cls.fullName.stripModuleClassSuffix.toString
    if (blackList contains path)
      debug_println(s"blacklisted: $path")
    else {
      debug_println(s"showing $path -> ${cls.denot}")
      val cinfo = cls.info
      val infoStr = if (cinfo.exists) cinfo.show else " is missing"
      debug_println("======================================")
      debug_println(cls.show + infoStr)
    }
  }

  def showClasses(path: String)(implicit ctx: Context): Unit = doTwice { implicit ctx =>
    debug_println(s"showing file $path")
    val cls = ctx.requiredClass(path.toTypeName)
    showClass(cls)
    showClass(cls.linkedClass)
  }
/*
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
  def loadScalaCollection() = {
    showPackage(ctx.requiredPackage("scala.collection"))
  }
*/
  /*@Test
  def showScala() = {
    showPackage("scala", 1)
  } */
  // ping @odersky dotty.tools.dotc.core.Types$CyclicReference: cyclic reference involving class AnyVals, took 1.303 sec
  //

  @Test
  def loadDotty() = {
    showPackage("dotty", 5)
  }


  /*
   * @Test
  def showReflectAliases() = { // tests for cycles during findMember
    showClasses("scala.reflect.macros.runtime.Aliases")
  }*/
}
