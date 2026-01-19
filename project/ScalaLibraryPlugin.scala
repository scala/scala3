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

import org.objectweb.asm.*

import dotty.tools.tasty.TastyHeaderUnpickler

object ScalaLibraryPlugin extends AutoPlugin {

  override def trigger = noTrigger
  override def requires: Plugins = MissingLinkPlugin

  /** Version of the compatible Scala 2.13 scala-library
   *  Should be updated when we synchronize with the sources of Scala 2.
   *
   *  This version would be used to fetch sources of Scala 2.13 standard library to be used for patching the Scala 3 standard library.
   */
  val scala2Version      = "2.13.18"

  /** Scala 2 pickle annotation descriptors that should be stripped from class files */
  private val Scala2PickleAnnotations = Set(
    "Lscala/reflect/ScalaSignature;",
    "Lscala/reflect/ScalaLongSignature;"
  )

  /** Scala 2 attribute names that should be stripped from class files */
  private val Scala2PickleAttributes = Set("ScalaSig", "ScalaInlineInfo")

  /** Check if an annotation descriptor is a Scala 2 pickle annotation */
  private def isScala2PickleAnnotation(descriptor: String): Boolean =
    Scala2PickleAnnotations.contains(descriptor)

  object autoImport {
    val scala2LibraryClasspath = taskKey[Vector[File]]("Jars containing Scala library artifacts to be used when patching")
  }

  import autoImport._


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
        if filesToCopy.exists(s => path == s || path.startsWith(s + '$')) // Only Override Some Very Specific Files
        dest = classDir / (id.toString)
        ref <- dest.relativeTo((LocalRootProject / baseDirectory).value)
      } {
        log.debug(s"Replacing generated .class file with Scala 2.13 patch: ${id}")
        patchFile(input = file, output = dest, classDirectory = classDir)
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
          patchFile(
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

  /** Remove Scala 2 Pickles from class file and optionally add TASTY attribute.
   *  Also ensures the Scala attribute is present for all Scala-compiled classes.
   *
   *  @param bytes the class file bytecode
   *  @param tastyUUID optional 16-byte UUID from the corresponding .tasty file (only for primary class)
   */
  private def patchClassFile(bytes: Array[Byte], tastyUUID: Option[Array[Byte]]): Array[Byte] = {
    val reader = new ClassReader(bytes)
    val writer = new ClassWriter(0)
    // Remove Scala 2 pickles and Scala signatures
    val visitor = new ClassVisitor(Opcodes.ASM9, writer) {
      override def visitAttribute(attr: Attribute): Unit = {
        val shouldRemove = Scala2PickleAttributes.contains(attr.`type`)
        if (!shouldRemove) super.visitAttribute(attr)
      }

      override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor =
        if (isScala2PickleAnnotation(desc)) null
        else super.visitAnnotation(desc, visible)
    }
    reader.accept(visitor, 0)
    // Only add TASTY attribute for the primary class (not for inner/nested classes)
    tastyUUID
      .map(new TastyAttribute(_))
      .foreach(writer.visitAttribute)
    // Add Scala attribute if not present and this is a Scala-compiled class
    def isJavaSourced = extractSourceFile(bytes).exists(_.endsWith(".java"))
    if (!hasScalaAttribute(bytes) && !isJavaSourced) {
      writer.visitAttribute(new ScalaAttribute)
    }
    writer.toByteArray
  }

  /** Apply the patches to given input file and write the result to the output.
   *  For .class files, strips Scala 2 pickles and adds TASTY attribute only for primary classes.
   *
   *  The TASTY attribute is only added to the "primary" class for each .tasty file:
   *  - Inner/nested classes (e.g., Outer$Inner.class) don't get TASTY attribute
   *  - Companion objects (Foo$.class when Foo.class exists) don't get TASTY attribute
   *  - Only the class whose name matches the .tasty file name gets the attribute
   *  - Java source files don't produce .tasty files, so they are skipped
   *
   *  Additionally validates that if the original class file (before patching) had a TASTY
   *  attribute, the patched version will also have one. This prevents accidentally losing
   *  TASTY attributes during the patching process.
   *
   *  @param input the input file (.class or .sjsir)
   *  @param output the output file location
   *  @param classDirectory the class directory to look for .tasty files
   */
  def patchFile(input: File, output: File, classDirectory: File): File = {
    if (input.getName.endsWith(".sjsir")) {
      // For .sjsir files, we just copy the file
      IO.copyFile(input, output)
      return output
    }

    // Extract the original TASTY UUID if the class file exists and has one
    val originalTastyUUID: Option[Array[Byte]] =
      if (output.exists()) extractTastyUUIDFromClass(IO.readBytes(output))
      else None

    val relativePath = output.relativeTo(classDirectory)
      .getOrElse(sys.error(s"Patched file is not relative to class directory: $output"))
      .getPath
    val classPath = relativePath.stripSuffix(".class")
    val basePath = classPath.split('$').head

    // Skip TASTY handling for Java-sourced classes (they don't have .tasty files)
    val classfileBytes = IO.readBytes(input)
    val isJavaSourced = extractSourceFile(classfileBytes).exists(_.endsWith(".java"))
    val tastyUUID =
      if (isJavaSourced) None
      else {
        val tastyFile = classDirectory / (basePath + ".tasty")
        assert(tastyFile.exists(), s"TASTY file $tastyFile does not exist for $relativePath")

        // Only add TASTY attribute if this is the primary class (class path equals base path)
        // Inner classes, companion objects ($), anonymous classes ($$anon), etc. don't get TASTY attribute
        val isPrimaryClass = classPath == basePath
        if (isPrimaryClass) Some(extractTastyUUID(IO.readBytes(tastyFile)))
        else None
    }

    // Validation to ensure that no new TASTY attributes are added or removed when compared with unpatched sources
    (tastyUUID, originalTastyUUID) match {
      case (None, None) => () // no TASTY attribute, no problem
      case (Some(newUUID), Some(originalUUID)) =>
        assert(java.util.Arrays.equals(originalUUID, newUUID),
          s"TASTY UUID mismatch for $relativePath: original=${originalUUID.map(b => f"$b%02x").mkString}, new=${newUUID.map(b => f"$b%02x").mkString}."
        )
      case (Some(_), None) => sys.error(s"TASTY attribute defined, but not present in unpatched source $relativePath")
      case (None, Some(_)) => sys.error(s"TASTY attribute missing, but present in unpatched $relativePath")
    }

    IO.write(output, patchClassFile(classfileBytes, tastyUUID))
    output
  }

  /** Check if class file bytecode contains Scala 2 pickle annotations or attributes */
  private def hasScala2Pickles(bytes: Array[Byte]): Boolean = {
    var hasPickleAnnotation = false
    var hasScalaSigAttr = false
    var hasScalaInlineInfoAttr = false
    val visitor = new ClassVisitor(Opcodes.ASM9) {
      override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor = {
        if (isScala2PickleAnnotation(desc)) hasPickleAnnotation = true
        null
      }
      override def visitAttribute(attr: Attribute): Unit =
        if (Scala2PickleAttributes.contains(attr.`type`)) attr.`type` match {
          case "ScalaSig" => hasScalaSigAttr = true
          case "ScalaInlineInfo" => hasScalaInlineInfoAttr = true
        }
    }
    new ClassReader(bytes).accept(
      visitor,
      ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES
    )
    hasPickleAnnotation || hasScalaSigAttr || hasScalaInlineInfoAttr
  }

  /** Check if class file bytecode contains a Scala attribute */
  private def hasScalaAttribute(bytes: Array[Byte]): Boolean = {
    var hasScala = false
    val visitor = new ClassVisitor(Opcodes.ASM9) {
      override def visitAttribute(attr: Attribute): Unit = {
        if (attr.`type` == "Scala") hasScala = true
      }
    }
    new ClassReader(bytes).accept(
      visitor,
      ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES
    )
    hasScala
  }

  /** Validate that all files produced by Scala compiler have a "Scala" attribute.
   *  Java-sourced files are excluded from this check since they don't have Scala attributes.
   */
  def validateScalaAttributes(jar: File): Unit = {
    val classFilesWithoutScala = Using.jarFile(verify = true)(jar) { jarFile =>
      jarFile
        .entries().asScala
        .filter(_.getName.endsWith(".class"))
        .flatMap { entry =>
          Using.bufferedInputStream(jarFile.getInputStream(entry)) { inputStream =>
            val bytes = inputStream.readAllBytes()
            // Skip Java-sourced files - they won't have Scala attributes
            val isJavaSourced = extractSourceFile(bytes).exists(_.endsWith(".java"))
            if (!isJavaSourced && !hasScalaAttribute(bytes)) Some(entry.getName)
            else None
          }
        }
        .toList
    }
    assert(
      classFilesWithoutScala.isEmpty,
      s"JAR ${jar.getName} contains ${classFilesWithoutScala.size} class files without 'Scala' attribute: ${classFilesWithoutScala.mkString("\n  - ", "\n  - ", "")}"
    )
  }

  def validateNoScala2Pickles(jar: File): Unit = {
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
  }

  private lazy val filesToCopy = Set(
    "scala/Tuple1",
    "scala/Tuple2",
    "scala/collection/ArrayOps$ArrayIterator",
    "scala/collection/ArrayOps$ReverseIterator",
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
    "scala/runtime/NonLocalReturnControl",
    "scala/util/hashing/MurmurHash3",
    "scala/util/Sorting",
  )

  /** Extract the SourceFile attribute from class file bytecode */
  private def extractSourceFile(bytes: Array[Byte]): Option[String] = {
    var sourceFile: Option[String] = None
    val visitor = new ClassVisitor(Opcodes.ASM9) {
      override def visitSource(source: String, debug: String): Unit =
        sourceFile = Option(source)
    }
    // Note: Don't use SKIP_DEBUG here - SourceFile is debug info and would be skipped
    new ClassReader(bytes).accept(
      visitor,
      ClassReader.SKIP_CODE | ClassReader.SKIP_FRAMES
    )
    sourceFile
  }

  /** Extract the UUID bytes (16 bytes) from a TASTy file.
   *
   *  Uses the official TastyHeaderUnpickler to parse the header and extract the UUID,
   *  ensuring correctness and validating the TASTy format.
   */
  private def extractTastyUUID(tastyBytes: Array[Byte]): Array[Byte] = {
    val unpickler = new TastyHeaderUnpickler(tastyBytes)
    val header = unpickler.readFullHeader()
    val uuid = header.uuid

    // Convert UUID (two longs) to 16-byte array in big-endian format
    val buffer = ByteBuffer.allocate(16)
    buffer.putLong(uuid.getMostSignificantBits)
    buffer.putLong(uuid.getLeastSignificantBits)
    buffer.array()
  }

  /** Extract TASTY UUID from class file bytecode, if present */
  private def extractTastyUUIDFromClass(bytes: Array[Byte]): Option[Array[Byte]] = {
    var result: Option[Array[Byte]] = None
    val tastyPrototype = new Attribute("TASTY") {
      override def read(cr: ClassReader, off: Int, len: Int, buf: Array[Char], codeOff: Int, labels: Array[Label]): Attribute = {
        if (len == 16) {
          val uuid = Array.tabulate[Byte](16)(i => cr.readByte(off + i).toByte)
          result = Some(uuid)
        }
        this
      }
    }
    new ClassReader(bytes).accept(
      new ClassVisitor(Opcodes.ASM9) {},
      Array(tastyPrototype),
      0
    )
    result
  }

  /** Validate TASTY attributes in the JAR:
   *  - If a .class file has a TASTY attribute, verify its UUID matches a .tasty file in the JAR
   *  - Every .tasty file must have at least one .class file with a matching TASTY attribute
   *
   *  Note: .class files from Java sources don't have .tasty files and are allowed to not have TASTY attributes.
   */
  def validateTastyAttributes(jar: File): Unit = {
    Using.jarFile(verify = true)(jar) { jarFile =>
      // Build a map of .tasty file paths to their UUIDs
      val tastyEntries = jarFile.entries().asScala
        .filter(_.getName.endsWith(".tasty"))
        .map { entry =>
          val bytes = Using.bufferedInputStream(jarFile.getInputStream(entry))(_.readAllBytes())
          val uuid = extractTastyUUID(bytes)
          entry.getName -> uuid
        }
        .toMap

      val errors = mutable.ListBuffer.empty[String]
      val referencedTastyFiles = mutable.Set.empty[String]

      // Check each .class file that has a TASTY attribute
      jarFile.entries().asScala
        .filter(e => e.getName.endsWith(".class"))
        .foreach { entry =>
          val classBytes = Using.bufferedInputStream(jarFile.getInputStream(entry))(_.readAllBytes())
          val classPath = entry.getName

          // Only validate classes that have a TASTY attribute
          extractTastyUUIDFromClass(classBytes).foreach[Unit] { classUUID =>
            // Find a .tasty file with matching UUID
            tastyEntries.find{ case (path, tastyUUID) =>
            java.util.Arrays.equals(classUUID, tastyUUID) && {
              val tastyName = file(path).getName().stripSuffix(".tasty")
              val className = file(entry.getName()).getName().stripSuffix(".class")
              // apparently 2 files might have the same UUID, e.g. param.scala and field.scala
              className.startsWith(tastyName)
            }} match {
              case Some((path, _)) =>
                referencedTastyFiles += path
              case None =>
                val uuidHex = classUUID.map(b => f"$b%02x").mkString
                errors += s"$classPath: has TASTY attribute (UUID=$uuidHex) but no matching .tasty file found in JAR"
            }
          }
        }

      // Check that every .tasty file has at least one .class file referencing it
      val unreferencedTastyFiles = tastyEntries.keySet -- referencedTastyFiles
      unreferencedTastyFiles.foreach { tastyPath =>
        errors += s"$tastyPath: no .class file with matching TASTY attribute found"
      }

      assert(
        errors.isEmpty,
        s"JAR ${jar.getName} has ${errors.size} TASTY validation errors:\n  - ${errors.mkString("\n  - ")}"
      )
    }
  }

  /** Custom ASM Attribute for TASTY that can be written to class files */
  private class TastyAttribute(val uuid: Array[Byte]) extends Attribute("TASTY") {
    override def write(classWriter: ClassWriter, code: Array[Byte], codeLength: Int, maxStack: Int, maxLocals: Int): ByteVector = {
      val bv = new ByteVector(uuid.length)
      bv.putByteArray(uuid, 0, uuid.length)
      bv
    }
  }

  /** Custom ASM Attribute for Scala attribute marker (empty attribute) */
  private class ScalaAttribute extends Attribute("Scala") {
    override def write(classWriter: ClassWriter, code: Array[Byte], codeLength: Int, maxStack: Int, maxLocals: Int): ByteVector = {
      // Scala attribute is empty (length = 0x0)
      new ByteVector(0)
    }
  }

  def fetchScalaJsScalaLibrary: Def.Initialize[Task[Vector[File]]] = Def.task {
    val stream = streams.value
    val target = (Compile / classDirectory).value
    val lm = dependencyResolution.value
    val log = stream.log
    val cache  = stream.cacheDirectory
    val retrieveDir = cache / "scalajs-scalalib" / scalaVersion.value

    val scalalibArtifact = "org.scala-js" % "scalajs-scalalib_2.13" % s"$scala2Version+$scalaJSVersion"
    lm.retrieve(scalalibArtifact, scalaModuleInfo = None, retrieveDir, log)
        .fold(w => throw w.resolveException, identity)
        .filterNot(_.getPath().contains("javalib"))
        .distinct
    }

  def fetchScala2LibrarySources(targetDirectory: SettingKey[File]): Def.Initialize[Task[File]] = Def.task {
    val version = scala2Version

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
      IO.unzip(
        from = scalaLibSourcesJar,
        toDirectory = targetDir,
        filter = new SimpleFilter(path => !excludedSourceFiles.contains(path.replace(File.separatorChar, '/')))
      )
    }(Set(scalaLibSourcesJar))
    targetDir
  }
}

object MissingLinkFilters {
  val excludedClassFiles = Set(
    // All these are copied from Scala 2,
    // it should never be reachable unless code has heavily inlined
    // which itself is not binary compatible
    "scala.Enumeration$ValueSet$$anon$1",
    "scala.collection.Iterator$$anon$3$$anon$4$$anon$5",
    "scala.collection.Iterator$$anon$3$$anon$4",
    "scala.collection.LazyZip2$$anon$7$$anon$8",
    "scala.collection.LazyZip3$$anon$15$$anon$16",
    "scala.collection.LazyZip4$$anon$23$$anon$24",
    "scala.collection.immutable.RedBlackTree$partitioner$1$",
    "scala.math.Ordering$$anonfun$orElse$2",
    "scala.math.Ordering$$anonfun$orElseBy$2",
    "scala.util.control.Exception$$anonfun$pfFromExceptions$1",
  )
}
