package dotty.tools.sbtplugin

import sbt.*
import sbt.Keys.*
import scala.jdk.CollectionConverters.*
import java.nio.file.Files

object ScalaLibraryPlugin extends AutoPlugin {

  override def trigger = noTrigger

  val fetchScala2ClassFiles = taskKey[(Set[File], File)]("Fetch the files to use that were compiled with Scala 2")
  //val scala2LibraryVersion  = settingKey[String]("Version of the Scala 2 Standard Library")

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
    (Compile / compile) := {
      val stream = streams.value
      val target = (Compile / classDirectory).value
      val (files, reference) = fetchScala2ClassFiles.value;
      val analysis = (Compile / compile).value
      stream.log.info(s"Copying files from Scala 2 Standard Library to $target")
      for (file <- files; id <- file.relativeTo(reference).map(_.toString())) {
        if (filesToCopy(id)) {
          stream.log.debug(s"Copying file '${id}' to ${target / id}")
          IO.copyFile(file, target / id)
        }
      }

      val overwrittenBinaries = Files.walk((Compile / classDirectory).value.toPath())
        .iterator()
        .asScala
        .map(_.toFile)
        .map(_.relativeTo((Compile / classDirectory).value).get)
        .toSet
      val diff = files.filterNot(_.relativeTo(reference).exists(overwrittenBinaries))

      IO.copy(diff.map { file =>
        file -> (Compile / classDirectory).value / file.relativeTo(reference).get.getPath
      })

      analysis
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
