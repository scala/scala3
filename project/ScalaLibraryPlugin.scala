package dotty.tools.sbtplugin

import sbt.*
import sbt.Keys.*
import sbt.io.Using
import sbt.librarymanagement.ModuleFilter
import scala.collection.mutable
import java.io.File
import java.nio.file.Files
import java.nio.ByteBuffer
import xsbti.VirtualFileRef
import sbt.internal.inc.Stamper
import scala.jdk.CollectionConverters.*
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.scalaJSVersion
import ch.epfl.scala.sbtmissinglink.MissingLinkPlugin
import ch.epfl.scala.sbtmissinglink.MissingLinkPlugin.autoImport.missinglinkCheck
import com.spotify.missinglink.Conflict

import dotty.tools.tasty.TastyHeaderUnpickler

object ScalaLibraryPlugin extends AutoPlugin {

  override def trigger = noTrigger
  override def requires: Plugins = MissingLinkPlugin

  object autoImport {
    val scala2LibraryClasspath = taskKey[Vector[File]]("Jars containing Scala library artifacts to be used when patching")
  }

  import autoImport._

  import ScalaJarValidate._


  override def projectSettings = Seq(
    // Settings to validate that JARs don't contain Scala 2 pickle annotations and have valid TASTY attributes
    Compile / packageBin := (Compile / packageBin)
      .map{ jar =>
        validateNoScala2Pickles(jar)
        validateTastyAttributes(jar)
        validateScalaAttributes(jar)
        jar
      }
      .value,
    (Compile / manipulateBytecode) := {
      val stream = streams.value
      val log = stream.log
      val classDir = (Compile / classDirectory).value

      // Fetch classfiles and sjsir files
      val patches: Seq[(Set[File], File)] = scala2LibraryClasspath.value.map(fetch(stream, _))
      val previous = (Compile / manipulateBytecode).value

      val analysis = previous.analysis match {
        case analysis: sbt.internal.inc.Analysis => analysis
        case _ => sys.error("Unexpected analysis type")
      }
      var stamps = analysis.stamps

      // Patch the files that are in the list
      for {
        (files, reference) <- patches
        file <- files.toSeq.sorted
        id <- file.relativeTo(reference)
        path = id.toString().replace("\\", "/").stripSuffix(".class").stripSuffix(".sjsir")
        if ScalaLibraryFilesToCopy.filesToCopy.exists(s => path == s || path.startsWith(s + '$')) // Only Override Some Very Specific Files
        dest = classDir / (id.toString)
        ref <- dest.relativeTo((LocalRootProject / baseDirectory).value)
      } {
        log.debug(s"Replacing generated .class file with Scala 2.13 patch: ${id}")
        StripScala2Annotations.patchFile(input = file, output = dest, classDirectory = classDir)
        // Update the timestamp in the analysis
        stamps = stamps.markProduct(
          VirtualFileRef.of(s"$${BASE}/$ref"),
          Stamper.forFarmHashP(dest.toPath()))
      }


      val overwrittenBinaries = Files.walk(classDir.toPath())
        .iterator()
        .asScala
        .map(_.toFile)
        .map(_.relativeTo(classDir).get)
        .toSet

      for ((files, reference) <- patches) {
        val diff = files.filterNot(file => overwrittenBinaries.contains(file.relativeTo(reference).get))
        log.debug(s"Found ${diff.size} .class files specific to Scala 2 in scala-library")
        // Copy all the specialized classes in the stdlib
        // no need to update any stamps as these classes exist nowhere in the analysis
        for (orig <- diff.toSeq.sorted; dest <- orig.relativeTo(reference)) {
          log.debug(s"Adding Scala 2.13 specific .class file: ${dest}")
          StripScala2Annotations.patchFile(
            input = orig,
            output = classDir / dest.toString(),
            classDirectory = classDir,
          )
        }
      }

      previous
        .withAnalysis(analysis.copy(stamps = stamps)) // update the analysis with the correct stamps
        .withHasModified(true)  // mark it as updated for sbt to update its caches
    },
    // The default sbt plugin has no way to filter out problems by class
    // We need to redefine it which requires reflective access
    Compile / missinglinkCheck := {
      val log = streams.value.log
      val cp = (Compile / fullClasspath).value
      val classDir = (Compile / classDirectory).value

      val conflicts: Seq[Conflict] = {
        val method = MissingLinkPlugin.getClass.getDeclaredMethods()
          .find(_.getName == "loadArtifactsAndCheckConflicts")
          .getOrElse(sys.error("MissingLinkPlugin.loadArtifactsAndCheckConflicts not found"))
        method.setAccessible(true)
        method.invoke(MissingLinkPlugin, cp, classDir, java.lang.Boolean.FALSE, (_ => true):ModuleFilter, log)
          .asInstanceOf[Seq[Conflict]]
      }

      val filteredConflicts = conflicts.filterNot { conflict =>
        MissingLinkFilters.excludedClassFiles.contains(
          conflict.dependency().fromClass().getClassName()
        )
      }

      if (filteredConflicts.isEmpty) {
        log.info(s"No conflicts found, filtered out ${conflicts.size} problems.")
      } else {
        val filteredTotal = filteredConflicts.length
        log.error(s"$filteredTotal conflicts found!")
        locally {
          val method = MissingLinkPlugin.getClass.getDeclaredMethods()
            .find(_.getName == "outputConflicts")
            .getOrElse(sys.error("MissingLinkPlugin.outputConflicts not found"))
          method.setAccessible(true)
          method.invoke(MissingLinkPlugin, filteredConflicts, log)
        }
        throw new MessageOnlyException(s"There were $filteredTotal conflicts")
      }
    }
  )

  def fetch(stream: TaskStreams, jar: File) = {
    val cache  = stream.cacheDirectory
    val target = cache / jar.getName()

    if (!target.exists()) {
      IO.createDirectory(target)
    }

    (FileFunction.cached(cache / "fetch-scala-library-classes", FilesInfo.lastModified, FilesInfo.exists) { _ =>
      stream.log.info(s"Unpacking scala-library binaries to persistent directory: ${target.getAbsolutePath}")
      IO.unzip(jar, target)
      (target ** "*.class").get.toSet ++ (target ** "*.sjsir").get.toSet
    } (Set(jar)), target)
  }

  def fetchScalaJsScalaLibrary: Def.Initialize[Task[Vector[File]]] = Def.task {
    val stream = streams.value
    val target = (Compile / classDirectory).value
    val lm = dependencyResolution.value
    val log = stream.log
    val cache  = stream.cacheDirectory
    val retrieveDir = cache / "scalajs-scalalib" / scalaVersion.value

    val scalalibArtifact = "org.scala-js" % "scalajs-scalalib_2.13" % s"${Versions.scala2Version}+$scalaJSVersion"
    lm.retrieve(scalalibArtifact, scalaModuleInfo = None, retrieveDir, log)
        .fold(w => throw w.resolveException, identity)
        .filterNot(_.getPath().contains("javalib"))
        .distinct
    }

  def fetchScala2LibrarySources(targetDirectory: SettingKey[File]): Def.Initialize[Task[File]] = Def.task {
    val version = Versions.scala2Version

    val targetDir = targetDirectory.value
    val stream = streams.value
    val log = stream.log
    val cache  = stream.cacheDirectory
    val scalaStdLibraryDep = "org.scala-lang" % "scala-library" % version
    lazy val scalaLibSourcesJar =
      dependencyResolution.value
        .retrieve(
          scalaStdLibraryDep.classifier("sources"),
          scalaModuleInfo = None, retrieveDirectory = cache, log = log
        )
        .getOrElse(sys.error(s"Could not fetch ${scalaStdLibraryDep} sources for version $version"))
        .find(_.name.endsWith(s"scala-library-$version-sources.jar"))
        .getOrElse(sys.error(s"Not expected .jar file in retrived dependencies"))

    // Auxilary source files that cannot be compield
    val excludedSourceFiles = Set(
        "scala/AnyRef.scala",
        "scala/Any.scala",
        "scala/Nothing.scala",
        "scala/Null.scala",
        "scala/Singleton.scala",
    )
    FileFunction.cached(
      cache / s"scala-library-sources-$version",
      FilesInfo.lastModified,
      FilesInfo.exists
    ) { _ =>
      log.debug(s"Unpacking Scala ${version} library sources to $targetDir...")
      if (targetDir.exists)
        IO.delete(targetDir)
      IO.createDirectory(targetDir)
      val unpacked = IO.unzip(
        from = scalaLibSourcesJar,
        toDirectory = targetDir,
        filter = new SimpleFilter(path => !excludedSourceFiles.contains(path.replace(File.separatorChar, '/')))
      )
      // Patch Scala 2's `Function2..Function22` sources to add `andThen`,
      // mirroring scala/scala#11121. The Scala 2 backport lands in a separate PR;
      // until then we inject the method here so that `Function{N}.class` produced
      // by the `scala2-library` project (which replaces our compiled output for
      // specialized synthetics) carries the new method.
      patchScala2FunctionSourcesForAndThen(targetDir, log)
      // Patch Scala 2's `HashMap.scala` to drop the `Function1[(K, V1), Unit]`
      // mixin from its private `accum` class — once `Function2` carries `andThen`,
      // mixing in both `Function1` and `Function2` causes a conflicting
      // implementations clash (same fix as in our Scala 3 `HashMap.scala`).
      patchScala2HashMapForAndThen(targetDir, log)
      unpacked
    }(Set(scalaLibSourcesJar))
    targetDir
  }

  private def patchScala2FunctionSourcesForAndThen(targetDir: File, log: Logger): Unit = {
    for (n <- 2 to 22) {
      val file = targetDir / "scala" / s"Function$n.scala"
      if (file.isFile) {
        val content = IO.read(file)
        if (!content.contains("def andThen[")) {
          val tParams = (1 to n).map(i => s"T$i").mkString(", ")
          val tParamsDecl = (1 to n).map(i => s"v$i: T$i").mkString(", ")
          val vArgs = (1 to n).map(i => s"v$i").mkString(", ")
          val andThen =
            s"""  /** Composes two instances of `Function$n` in a new `Function$n`, with this function applied first.
               | *
               | *  @tparam A the result type of function `g`
               | *  @param  g a function R => A
               | *  @return a new function `f` such that `f(x1, ..., x$n) == g(apply(x1, ..., x$n))`
               | */
               |  @annotation.unspecialized def andThen[A](g: R => A): ($tParams) => A = { ($tParamsDecl) => g(apply($vArgs)) }
               |""".stripMargin
          // Inject before the existing `curried` definition, which is present in every Function2..22.
          val curriedMarker = "  @annotation.unspecialized def curried"
          val idx = content.indexOf(curriedMarker)
          if (idx < 0) {
            log.warn(s"Could not locate `curried` in $file to inject `andThen`; leaving file untouched.")
          } else {
            val patched = content.substring(0, idx) + andThen + content.substring(idx)
            IO.write(file, patched)
            log.debug(s"Patched $file to add `andThen`.")
          }
        }
      }
    }
  }

  private def patchScala2HashMapForAndThen(targetDir: File, log: Logger): Unit = {
    val file = targetDir / "scala" / "collection" / "immutable" / "HashMap.scala"
    if (!file.isFile) return
    val content = IO.read(file)
    val classDecl = "class accum extends AbstractFunction2[K, V1, Unit] with Function1[(K, V1), Unit] {"
    val applyKv = "        def apply(kv: (K, V1)) = apply(kv._1, kv._2)"
    val foreachAccum = "            it.foreach(accum)"
    if (!content.contains(classDecl)) return // already patched or layout changed
    val patched = content
      .replace(classDecl, "class accum extends AbstractFunction2[K, V1, Unit] {")
      .replace(applyKv + "\n", "")
      .replace(
        foreachAccum,
        """            while (it.hasNext) {
          |              val (k, v) = it.next()
          |              accum(k, v)
          |            }""".stripMargin
      )
    if (patched != content) {
      IO.write(file, patched)
      log.debug(s"Patched $file to drop `Function1` mixin from class accum.")
    } else {
      log.warn(s"Failed to patch $file for `accum` dual-inheritance fix; layout may have changed.")
    }
  }
}
