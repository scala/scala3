package dotty.tools.sbtplugin

import sbt.*
import sbt.Keys.*
import sbt.io.Using
import scala.jdk.CollectionConverters.*
import java.nio.file.Files
import xsbti.VirtualFileRef
import sbt.internal.inc.Stamper
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.scalaJSVersion
import org.objectweb.asm.*

object ScalaLibraryPlugin extends AutoPlugin {

  override def trigger = noTrigger

  /** Version of the compatible Scala 2.13 scala-library
   *  Should be updated when we synchronize with the sources of Scala 2.
   *
   *  This version would be used to fetch sources of Scala 2.13 standard library to be used for patching the Scala 3 standard library.
   */
  val scala2Version = "2.13.18"

  /** Scala 2 pickle annotation descriptors that should be stripped from class files */
  private val Scala2PickleAnnotations = Set(
    "Lscala/reflect/ScalaSignature;",
    "Lscala/reflect/ScalaLongSignature;"
  )

  /** Check if an annotation descriptor is a Scala 2 pickle annotation */
  private def isScala2PickleAnnotation(descriptor: String): Boolean =
    Scala2PickleAnnotations.contains(descriptor)

  object autoImport {
    val keepSJSIR = settingKey[Boolean]("Should we patch .sjsir too?")
  }

  import autoImport._

  override def projectSettings = Seq (
    // Settings to validate that JARs don't contain Scala 2 pickle annotations
    Compile / packageBin := (Compile / packageBin)
      .map(validateNoScala2Pickles)
      .value,
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
        patchFile(input = file, output = dest)
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
          patchFile(
            input = orig,
            output = (Compile / classDirectory).value / dest.toString()
          )
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

  /* Remove Scala 2 Pickles from Classfiles */
  private def unpickler(bytes: Array[Byte]): Array[Byte] = {
    val reader = new ClassReader(bytes)
    val writer = new ClassWriter(0)
    val visitor = new ClassVisitor(Opcodes.ASM9, writer) {
      override def visitAttribute(attr: Attribute): Unit = attr.`type` match {
        case "ScalaSig" | "ScalaInlineInfo" => ()
        case _ => super.visitAttribute(attr)
      }

      override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor =
        if (isScala2PickleAnnotation(desc)) null
        else super.visitAnnotation(desc, visible)
    }
    reader.accept(visitor, 0)
    writer.toByteArray
  }

  // Apply the patches to given input file and write the result to the output
  def patchFile(input: File, output: File): File = {
    if (input.getName.endsWith(".class")) {
      IO.write(output, unpickler(IO.readBytes(input)))
    } else {
      // For .sjsir files, we just copy the file
      IO.copyFile(input, output)
    }
    output
  }

  /** Check if class file bytecode contains Scala 2 pickle annotations */
  private def hasScala2Pickles(bytes: Array[Byte]): Boolean = {
    var found = false
    val visitor = new ClassVisitor(Opcodes.ASM9) {
      override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor = {
        if (isScala2PickleAnnotation(desc)) found = true
        null
      }
    }
    new ClassReader(bytes).accept(
      visitor,
      ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES
    )
    found
  }

  def validateNoScala2Pickles(jar: File): File = {
    val classFilesWithPickles = Using.jarFile(verify = true)(jar){ jarFile =>
      jarFile
      .entries().asScala
      .filter(_.getName.endsWith(".class"))
      .flatMap { entry =>
        Using.bufferedInputStream(jarFile.getInputStream(entry)){ inputStream =>
          if (hasScala2Pickles(inputStream.readAllBytes())) Some(entry.getName)
          else None
        }
      }
      .toList
    }
    assert(
      classFilesWithPickles.isEmpty,
      s"JAR ${jar.getName} contains ${classFilesWithPickles.size} class files with Scala 2 pickle annotations: ${classFilesWithPickles.mkString("\n  - ", "\n  - ", "")}"
    )
    jar
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
