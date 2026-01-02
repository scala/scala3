package dotty.tools.sbtplugin

import sbt.*
import sbt.Keys.*
import scala.jdk.CollectionConverters.*
import java.nio.file.Files
import xsbti.VirtualFileRef
import sbt.internal.inc.Stamper
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.scalaJSVersion

object ScalaLibraryPlugin extends AutoPlugin {

  override def trigger = noTrigger

  /** Version of the compatible Scala 2.13 scala-library
   *  Should be updated when we synchronize with the sources of Scala 2.
   *
   *  This version would be used to fetch sources of Scala 2.13 standard library to be used for patching the Scala 3 standard library.
   */
  val scala2Version = "2.13.18"

  object autoImport {
    val keepSJSIR = settingKey[Boolean]("Should we patch .sjsir too?")
  }

  import autoImport._

  override def projectSettings = Seq (
    (Compile / manipulateBytecode) := {
      val stream = streams.value
      val target = (Compile / classDirectory).value
      val lm = dependencyResolution.value
      val log = stream.log
      val cache  = stream.cacheDirectory
      val retrieveDir = cache / "scalajs-scalalib" / scalaVersion.value

      val comp = lm.retrieve("org.scala-js" % "scalajs-scalalib_2.13" % s"$scala2Version+$scalaJSVersion", scalaModuleInfo = None, retrieveDir, log)
          .fold(w => throw w.resolveException, identity)
          .filterNot(_.getPath().contains("javalib"))
          .filter(!_.getPath().contains("scalajs-scalalib_2.13") || keepSJSIR.value)
          .distinct

      // Fetch classfiles and sjsir files
      val patches: Seq[(Set[File], File)] = comp.map(fetch(stream, _))

      val previous = (Compile / manipulateBytecode).value

      val analysis = previous.analysis match {
        case analysis: sbt.internal.inc.Analysis => analysis
        case _ => sys.error("Unexpected analysis type")
      }
      var stamps = analysis.stamps

      // Patch the files that are in the list
      for {
        (files, reference) <- patches
        file <- files
        id <- file.relativeTo(reference)
        path = id.toString().replace("\\", "/").stripSuffix(".class").stripSuffix(".sjsir")
        if filesToCopy.exists(s => path == s || path.startsWith(s + '$')) // Only Override Some Very Specific Files
        dest = target / (id.toString)
        ref <- dest.relativeTo((LocalRootProject / baseDirectory).value)
      } {
        // Copy the files to the classDirectory
        IO.copyFile(file, dest)
        // Update the timestamp in the analysis
        stamps = stamps.markProduct(
          VirtualFileRef.of(s"$${BASE}/$ref"),
          Stamper.forFarmHashP(dest.toPath()))
      }


      val overwrittenBinaries = Files.walk((Compile / classDirectory).value.toPath())
        .iterator()
        .asScala
        .map(_.toFile)
        .map(_.relativeTo((Compile / classDirectory).value).get)
        .toSet

      for ((files, reference) <- patches) {
        val diff = files.filterNot(file => overwrittenBinaries.contains(file.relativeTo(reference).get))
        // Copy all the specialized classes in the stdlib
        // no need to update any stamps as these classes exist nowhere in the analysis
        for (orig <- diff; dest <- orig.relativeTo(reference)) {
          IO.copyFile(orig, ((Compile / classDirectory).value / dest.toString()))
        }
      }

      previous
        .withAnalysis(analysis.copy(stamps = stamps)) // update the analysis with the correct stamps
        .withHasModified(true)  // mark it as updated for sbt to update its caches

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

  private lazy val filesToCopy = Set(
    "scala/Tuple1",
    "scala/Tuple2",
    "scala/collection/Stepper",
    "scala/collection/DoubleStepper",
    "scala/collection/IntStepper",
    "scala/collection/LongStepper",
    "scala/collection/immutable/DoubleVectorStepper",
    "scala/collection/immutable/IntVectorStepper",
    "scala/collection/immutable/LongVectorStepper",
    "scala/collection/immutable/Range",
    "scala/jdk/DoubleAccumulator",
    "scala/jdk/IntAccumulator",
    "scala/jdk/LongAccumulator",
    "scala/jdk/FunctionWrappers$FromJavaDoubleBinaryOperator",
    "scala/jdk/FunctionWrappers$FromJavaBooleanSupplier",
    "scala/jdk/FunctionWrappers$FromJavaDoubleConsumer",
    "scala/jdk/FunctionWrappers$FromJavaDoublePredicate",
    "scala/jdk/FunctionWrappers$FromJavaDoubleSupplier",
    "scala/jdk/FunctionWrappers$FromJavaDoubleToIntFunction",
    "scala/jdk/FunctionWrappers$FromJavaDoubleToLongFunction",
    "scala/jdk/FunctionWrappers$FromJavaIntBinaryOperator",
    "scala/jdk/FunctionWrappers$FromJavaDoubleUnaryOperator",
    "scala/jdk/FunctionWrappers$FromJavaIntPredicate",
    "scala/jdk/FunctionWrappers$FromJavaIntConsumer",
    "scala/jdk/FunctionWrappers$FromJavaIntSupplier",
    "scala/jdk/FunctionWrappers$FromJavaIntToDoubleFunction",
    "scala/jdk/FunctionWrappers$FromJavaIntToLongFunction",
    "scala/jdk/FunctionWrappers$FromJavaIntUnaryOperator",
    "scala/jdk/FunctionWrappers$FromJavaLongBinaryOperator",
    "scala/jdk/FunctionWrappers$FromJavaLongConsumer",
    "scala/jdk/FunctionWrappers$FromJavaLongPredicate",
    "scala/jdk/FunctionWrappers$FromJavaLongSupplier",
    "scala/jdk/FunctionWrappers$FromJavaLongToDoubleFunction",
    "scala/jdk/FunctionWrappers$FromJavaLongToIntFunction",
    "scala/jdk/FunctionWrappers$FromJavaLongUnaryOperator",
    "scala/collection/ArrayOps$ReverseIterator",
    "scala/runtime/NonLocalReturnControl",
    "scala/util/Sorting",
    )

}
