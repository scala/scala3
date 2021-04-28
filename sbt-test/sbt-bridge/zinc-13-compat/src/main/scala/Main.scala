
import java.net.URLClassLoader
import java.io.File
import java.util.{EnumSet, Set, HashSet}

import org.apache.logging.log4j.LogManager

import xsbti._
import xsbti.api.ClassLike
import xsbti.api.DependencyContext
import xsbti.compile.DependencyChanges

import sbt.internal.util.ManagedLogger
import sbt.internal.inc.FreshCompilerCache
import sbt.internal.inc.ZincUtil
import sbt.internal.inc.ScalaInstance

object Main extends App {
  val loaderLibraryOnly = new URLClassLoader(Scala3.libraryJars.map(_.toURI.toURL), null)
  val loader = new URLClassLoader(Scala3.allJars.map(_.toURI.toURL), loaderLibraryOnly)
  val instance = new ScalaInstance(
    Scala3.version,
    loader,
    loaderLibraryOnly,
    Scala3.libraryJars,
    Scala3.compilerJar,
    Scala3.allJars,
    Some(Scala3.version)
  )
  val compiler = ZincUtil.scalaCompiler(instance, Scala3.bridgeJar)

  val noChanges = new DependencyChanges {
    val modifiedBinaries = Array.empty[File]
    val modifiedClasses = Array.empty[String]
    def isEmpty(): Boolean = true
  }
  val noCallback = new AnalysisCallback {
    override def startSource(x$1: File): Unit = ()
    override def classDependency(x$1: String, x$2: String, x$3: DependencyContext): Unit = ()
    override def binaryDependency(x$1: File, x$2: String, x$3: String, x$4: File, x$5: DependencyContext): Unit = ()
    override def generatedNonLocalClass(x$1: File, x$2: File, x$3: String, x$4: String): Unit = ()
    override def generatedLocalClass(x$1: File, x$2: File): Unit = ()
    override def api(x$1: File, x$2: ClassLike): Unit = ()
    override def mainClass(x$1: File, x$2: String): Unit = ()
    override def usedName(x$1: String, x$2: String, x$3: EnumSet[UseScope]): Unit = ()
    override def problem(x$1: String, x$2: Position, x$3: String, x$4: Severity, x$5: Boolean): Unit = ()
    override def dependencyPhaseCompleted(): Unit = ()
    override def apiPhaseCompleted(): Unit = ()
    override def enabled(): Boolean = false
    override def classesInOutputJar(): Set[String] = new HashSet[String]()
  }
  val cache = new FreshCompilerCache()
  val log = new ManagedLogger("default", None, None, LogManager.getLogger())

  compiler.apply(
    Input.sources,
    noChanges,
    Scala3.libraryJars,
    Input.outputFile,
    options = Array(),
    noCallback,
    maximumErrors = 10,
    cache,
    log
  )
}



