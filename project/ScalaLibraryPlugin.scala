package dotty.tools.sbtplugin

import sbt.*
import sbt.Keys.*
import scala.jdk.CollectionConverters.*
import java.nio.file.Files
import xsbti.VirtualFileRef
import sbt.internal.inc.Stamper

object ScalaLibraryPlugin extends AutoPlugin {

  override def trigger = noTrigger

  val fetchScala2ClassFiles = taskKey[(Set[File], File)]("Fetch the files to use that were compiled with Scala 2")

  override def projectSettings = Seq (
    fetchScala2ClassFiles := {
      val stream = streams.value
      val cache  = stream.cacheDirectory
      val target = cache / "scala-library-classes"
      val report = update.value

      val scalaLibraryBinaryJar = report.select(
        configuration = configurationFilter(),
        module = (_: ModuleID).name == "scala-library",
        artifact = artifactFilter(`type` = "jar")).headOption.getOrElse {
          sys.error(s"Could not fetch scala-library binary JAR")
        }

      if (!target.exists()) {
        IO.createDirectory(target)
      }

      (FileFunction.cached(cache / "fetch-scala-library-classes", FilesInfo.lastModified, FilesInfo.exists) { _ =>
        stream.log.info(s"Unpacking scala-library binaries to persistent directory: ${target.getAbsolutePath}")
        IO.unzip(scalaLibraryBinaryJar, target)
        (target ** "*.class").get.toSet
      } (Set(scalaLibraryBinaryJar)), target)

    },
    (Compile / manipulateBytecode) := {
      val stream = streams.value
      val target = (Compile / classDirectory).value
      val (files, reference) = fetchScala2ClassFiles.value;
      val previous = (Compile / manipulateBytecode).value
      val analysis = previous.analysis match {
        case analysis: sbt.internal.inc.Analysis => analysis
        case _ => sys.error("Unexpected analysis type")
      }

      var stamps = analysis.stamps
      for (file <- files; 
           id <- file.relativeTo(reference); 
           if filesToCopy(id.toString()); // Only Override Some Very Specific Files
           dest = target / (id.toString); 
           ref <- dest.relativeTo((LocalRootProject / baseDirectory).value)
          ) { 
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

      val diff = files.filterNot(_.relativeTo(reference).exists(overwrittenBinaries))

      // Copy all the specialized classes in the stdlib
      // no need to update any stamps as these classes exist nowhere in the analysis
      for (orig <- diff; dest <- orig.relativeTo(reference)) {
        IO.copyFile(orig, ((Compile / classDirectory).value / dest.toString()))
      }

      previous
        .withAnalysis(analysis.copy(stamps = stamps)) // update the analysis with the correct stamps
        .withHasModified(true)  // mark it as updated for sbt to update its caches
    }
  )

  private lazy val filesToCopy = Set(
    "scala/Tuple1.class",
    "scala/Tuple2.class",
    "scala/collection/DoubleStepper.class",
    "scala/collection/IntStepper.class",
    "scala/collection/LongStepper.class",
    "scala/collection/immutable/DoubleVectorStepper.class",
    "scala/collection/immutable/IntVectorStepper.class",
    "scala/collection/immutable/LongVectorStepper.class",
    "scala/jdk/DoubleAccumulator.class",
    "scala/jdk/IntAccumulator.class",
    "scala/jdk/LongAccumulator.class",
    "scala/jdk/FunctionWrappers$FromJavaDoubleBinaryOperator.class",
    "scala/jdk/FunctionWrappers$FromJavaBooleanSupplier.class",
    "scala/jdk/FunctionWrappers$FromJavaDoubleConsumer.class",
    "scala/jdk/FunctionWrappers$FromJavaDoublePredicate.class",
    "scala/jdk/FunctionWrappers$FromJavaDoubleSupplier.class",
    "scala/jdk/FunctionWrappers$FromJavaDoubleToIntFunction.class",
    "scala/jdk/FunctionWrappers$FromJavaDoubleToLongFunction.class",
    "scala/jdk/FunctionWrappers$FromJavaIntBinaryOperator.class",
    "scala/jdk/FunctionWrappers$FromJavaDoubleUnaryOperator.class",
    "scala/jdk/FunctionWrappers$FromJavaIntPredicate.class",
    "scala/jdk/FunctionWrappers$FromJavaIntConsumer.class",
    "scala/jdk/FunctionWrappers$FromJavaIntSupplier.class",
    "scala/jdk/FunctionWrappers$FromJavaIntToDoubleFunction.class",
    "scala/jdk/FunctionWrappers$FromJavaIntToLongFunction.class",
    "scala/jdk/FunctionWrappers$FromJavaIntUnaryOperator.class",
    "scala/jdk/FunctionWrappers$FromJavaLongBinaryOperator.class",
    "scala/jdk/FunctionWrappers$FromJavaLongConsumer.class",
    "scala/jdk/FunctionWrappers$FromJavaLongPredicate.class",
    "scala/jdk/FunctionWrappers$FromJavaLongSupplier.class",
    "scala/jdk/FunctionWrappers$FromJavaLongToDoubleFunction.class",
    "scala/jdk/FunctionWrappers$FromJavaLongToIntFunction.class",
    "scala/jdk/FunctionWrappers$FromJavaLongUnaryOperator.class",
    "scala/collection/ArrayOps$ReverseIterator.class",
    "scala/runtime/NonLocalReturnControl.class",
    "scala/util/Sorting.class", "scala/util/Sorting$.class",  // Contains @specialized annotation
    )

}
