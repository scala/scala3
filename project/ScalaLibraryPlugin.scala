package dotty.tools.sbtplugin

import sbt.*
import sbt.Keys.*
import sbt.io.Using
import scala.jdk.CollectionConverters.*
import scala.collection.mutable
import java.nio.file.Files
import java.nio.ByteBuffer
import xsbti.VirtualFileRef
import sbt.internal.inc.Stamper
import org.scalajs.sbtplugin.ScalaJSPlugin.autoImport.scalaJSVersion
import org.objectweb.asm.*

import dotty.tools.tasty.TastyHeaderUnpickler

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

  /** Scala 2 attribute names that should be stripped from class files */
  private val Scala2PickleAttributes = Set("ScalaSig", "ScalaInlineInfo")

  /** Check if an annotation descriptor is a Scala 2 pickle annotation */
  private def isScala2PickleAnnotation(descriptor: String): Boolean =
    Scala2PickleAnnotations.contains(descriptor)

  object autoImport {
    val keepSJSIR = settingKey[Boolean]("Should we patch .sjsir too?")
  }

  import autoImport._

  override def projectSettings = Seq (
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
      val target = (Compile / classDirectory).value
      val lm = dependencyResolution.value
      implicit val log = stream.log
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

      val classDir = (Compile / classDirectory).value
      val baseDir = (LocalRootProject / baseDirectory).value

      // Patch the files that are in the list
      for {
        (files, reference) <- patches
        file <- files
        id <- file.relativeTo(reference)
        path = id.toString().replace("\\", "/").stripSuffix(".class").stripSuffix(".sjsir")
        if filesToCopy.exists(s => path == s || path.startsWith(s + '$')) // Only Override Some Very Specific Files
        dest = target / (id.toString)
        ref <- dest.relativeTo(baseDir)
      } {
        log.debug(s"Replacing class using Scala 2 artifact: $ref")
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
        // Copy all the specialized classes in the stdlib
        // no need to update any stamps as these classes exist nowhere in the analysis
        for (orig <- diff; dest <- orig.relativeTo(reference)) {
          log.debug("Copying Scala 2 specific class: " + dest)
          patchFile(
            input = orig,
            output = classDir / dest.toString(),
            classDirectory = classDir,
          )
        }
      }

      if (!keepSJSIR.value && analysis.stamps.products.isEmpty) {
        // No modified Scala.js bytecode, no need to check and warn. No rules would be every applied
        BytecodeRewrites.validateRewrittenTargetsPublic(classDir)
        BytecodeRewrites.rulesUsage.checkForUnusedRules(log)
        BytecodeRewrites.rulesUsage.reset()
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


  /** Remove Scala 2 Pickles from class file and optionally add TASTY attribute.
   *  Also ensures the Scala attribute is present for all Scala-compiled classes.
   *  Additionally rewrites incompatible field and method references in Scala 2 bytecode.
   *
   *  @param bytes the class file bytecode
   *  @param tastyUUID optional 16-byte UUID from the corresponding .tasty file (only for primary class)
   */
  private def patchClassFile(classFile: String, bytes: Array[Byte], tastyUUID: Option[Array[Byte]])(implicit log: Logger): Array[Byte] = {
    val reader = new ClassReader(bytes)
    val writer = new ClassWriter(0)
    // Remove Scala 2 pickles, Scala signatures, and rewrite incompatible field/method references
    val visitor = new ClassVisitor(Opcodes.ASM9, writer) {
      override def visitAttribute(attr: Attribute): Unit = {
        val shouldRemove = Scala2PickleAttributes.contains(attr.`type`)
        if (!shouldRemove) super.visitAttribute(attr)
      }

      override def visitAnnotation(desc: String, visible: Boolean): AnnotationVisitor =
        if (isScala2PickleAnnotation(desc)) null
        else super.visitAnnotation(desc, visible)

      // Chain bytecode rewriting visitor for method instructions
      override def visitMethod(access: Int, name: String, descriptor: String, signature: String, exceptions: Array[String]): MethodVisitor = {
        val mv = super.visitMethod(access, name, descriptor, signature, exceptions)
        if (mv != null) new BytecodeRewrites.ReferenceRewritingMethodVisitor(mv, classFile, log)
        else null
      }
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
  def patchFile(input: File, output: File, classDirectory: File)(implicit log: Logger): File = {
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

    IO.write(output, patchClassFile(relativePath, classfileBytes, tastyUUID))
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
}

private object BytecodeRewrites {
  import BytecodeRewrites.MemberToRewriteOps

  def rewriteTargetsByOwner: Map[String,Seq[MemberReference]] = rules.map{rule =>
      rule.rewrite match {
        case RewriteOperation.Rename(newName)     => rule.reference.copy(name = newName)
        case RewriteOperation.Delegate(target, _) => target
      }
    }.groupBy(_.owner)

  object rulesUsage {
    // Tracks usage of given rule by tracking to which classfiles it was applies
    private val usages = mutable.Map.empty[Rule, mutable.Set[String]]
    def reset() = usages.clear()
    def mark(rule: Rule, classFile: String): Unit = usages
      .getOrElseUpdate(rule, mutable.Set.empty)
      .update(classFile, included = true)

    def checkForUnusedRules(log: Logger) = {
      val unused = rules.flatMap { rule =>
        usages.get(rule) match {
          case None => Some(rule)
          case Some(appliedTo) if appliedTo.isEmpty => Some(rule)
          case _ => None
        }
      }.toSeq

      // Check for rules that were never triggered
      if (unused.size > 0) {
        log.warn(s"Found ${unused.size} unused bytecode rewrite rules:\n\t${unused.mkString("\n\t- ")}")
      }

      // check for rules aplpies less times then expected
      usages.foreach{
        case (rule, appliedTo) =>
          val diff = rule.appliesTo -- appliedTo
          if (diff.nonEmpty) {
          log.warn(s"Rewrite rule {ref:${rule.reference},rewrite:${rule.rewrite}} not applied to ${diff.size} expected files:\n\t* ${diff.mkString("\n\t* ")}")
        }
      }
    }
  }

  /** Field reference transformations that become method calls in Scala 3 */
  private val rules: Seq[Rule] = Seq(
    // Inlined direct access to `object Iterator: private[this] val _empty: Iterator._empty`
    // Usage in: Array.empty[Int].reverseIterator.next, Range(1, 1).grouped(1).next
    // Deoptimize by calling public accessor
    MemberReference("scala/collection/Iterator$", "scala$collection$Iterator$$_empty", "Lscala/collection/Iterator;")
      .delegateToAccessor("empty")
      .onOpCode(Opcodes.GETSTATIC)
      .applyTo(
        "scala/collection/ArrayOps$ArrayIterator.class",
        "scala/collection/ArrayOps$ArrayIterator$mcB$sp.class",
        "scala/collection/ArrayOps$ArrayIterator$mcC$sp.class",
        "scala/collection/ArrayOps$ArrayIterator$mcD$sp.class",
        "scala/collection/ArrayOps$ArrayIterator$mcF$sp.class",
        "scala/collection/ArrayOps$ArrayIterator$mcI$sp.class",
        "scala/collection/ArrayOps$ArrayIterator$mcJ$sp.class",
        "scala/collection/ArrayOps$ArrayIterator$mcS$sp.class",
        "scala/collection/ArrayOps$ArrayIterator$mcV$sp.class",
        "scala/collection/ArrayOps$ArrayIterator$mcZ$sp.class",
        "scala/collection/ArrayOps$ReverseIterator.class",
        "scala/collection/ArrayOps$ReverseIterator$mcB$sp.class",
        "scala/collection/ArrayOps$ReverseIterator$mcC$sp.class",
        "scala/collection/ArrayOps$ReverseIterator$mcD$sp.class",
        "scala/collection/ArrayOps$ReverseIterator$mcF$sp.class",
        "scala/collection/ArrayOps$ReverseIterator$mcI$sp.class",
        "scala/collection/ArrayOps$ReverseIterator$mcJ$sp.class",
        "scala/collection/ArrayOps$ReverseIterator$mcS$sp.class",
        "scala/collection/ArrayOps$ReverseIterator$mcV$sp.class",
        "scala/collection/ArrayOps$ReverseIterator$mcZ$sp.class",
        "scala/collection/immutable/Range$$anon$1.class",
        "scala/collection/immutable/Range$$anon$2.class",
        "scala/collection/immutable/Range$$anon$3.class",
        "scala/collection/immutable/Range.class",
      ),
    // Mangling of the fields has changed
    MemberReference("scala/collection/Iterator$$anon$3", "scala$collection$Iterator$$anon$$current", "Lscala/collection/Iterator;")
      .renameTo("scala$collection$Iterator$$anon$3$$current")
      .onOpCode(Opcodes.GETFIELD, Opcodes.PUTFIELD)
      .applyTo("scala/collection/Iterator$$anon$3$$anon$4.class"),
    // No longer z and op clousers were stored in fields in Scala 3, we've now forced adding these (in source) and renamed their accessor
    MemberReference("scala/collection/Iterator$$anon$3", "z$1", "Ljava/lang/Object;")
      .renameTo("scala$collection$Iterator$$anon$3$$_z")
      .onOpCode(Opcodes.GETFIELD)
      .applyTo(
        "scala/collection/Iterator$$anon$3$$anon$4.class",
        "scala/collection/Iterator$$anon$3$$anon$4$$anon$5.class"
      ),
    MemberReference("scala/collection/Iterator$$anon$3", "op$1", "Lscala/Function2;")
      .renameTo("scala$collection$Iterator$$anon$3$$_op")
      .onOpCode(Opcodes.GETFIELD)
      .applyTo("scala/collection/Iterator$$anon$3$$anon$4$$anon$5.class"),
    // Outer accessor methods: Scala 3 inserts underscore after class name
    MemberReference("scala/collection/Iterator$$anon$3", "scala$collection$Iterator$$anon$$$outer", "()Lscala/collection/Iterator;")
      .renameTo("scala$collection$Iterator$_$$anon$$$outer")
      .onOpCode(Opcodes.INVOKEVIRTUAL)
      .applyTo(
        "scala/collection/Iterator$$anon$3$$anon$4.class",
        "scala/collection/Iterator$$anon$3$$anon$4$$anon$5.class"
      ),
    // Inlined direct access to RedBlackTree$Tree instance private[immutable] fields:
    // Deoptimize by calling public accessor
    // object partitioner is local object defined inside RedBlackTree.partitionEntries, Scala 2 produces RedBlackTree$partitioner$1$.class, Scala 3 RedBlackTree$partitioner$2$.class,
    // Unlikely: requires inlining, can reproduce
    MemberReference("scala/collection/immutable/RedBlackTree$Tree", "scala$collection$immutable$RedBlackTree$Tree$$_key", "Ljava/lang/Object;")
      .delegateToAccessor("key")
      .onOpCode(Opcodes.GETFIELD)
      .applyTo("scala/collection/immutable/RedBlackTree$partitioner$1$.class"),
    MemberReference("scala/collection/immutable/RedBlackTree$Tree", "scala$collection$immutable$RedBlackTree$Tree$$_value", "Ljava/lang/Object;")
      .delegateToAccessor("value")
      .onOpCode(Opcodes.GETFIELD)
      .applyTo("scala/collection/immutable/RedBlackTree$partitioner$1$.class"),
    MemberReference("scala/collection/immutable/RedBlackTree$Tree", "scala$collection$immutable$RedBlackTree$Tree$$_left", "Lscala/collection/immutable/RedBlackTree$Tree;")
      .delegateToAccessor("left")
      .onOpCode(Opcodes.GETFIELD)
      .applyTo("scala/collection/immutable/RedBlackTree$partitioner$1$.class"),
    MemberReference("scala/collection/immutable/RedBlackTree$Tree", "scala$collection$immutable$RedBlackTree$Tree$$_right", "Lscala/collection/immutable/RedBlackTree$Tree;")
      .delegateToAccessor("right")
      .onOpCode(Opcodes.GETFIELD)
      .applyTo("scala/collection/immutable/RedBlackTree$partitioner$1$.class"),
    // Name mangling difference $$join vs $$$join
    MemberReference("scala/collection/immutable/RedBlackTree$", "scala$collection$immutable$RedBlackTree$$join", "(Lscala/collection/immutable/RedBlackTree$Tree;Ljava/lang/Object;Ljava/lang/Object;Lscala/collection/immutable/RedBlackTree$Tree;)Lscala/collection/immutable/RedBlackTree$Tree;")
      .renameTo("scala$collection$immutable$RedBlackTree$$$join")
      .onOpCode(Opcodes.INVOKEVIRTUAL)
      .applyTo("scala/collection/immutable/RedBlackTree$partitioner$1$.class"),
    MemberReference("scala/collection/immutable/RedBlackTree$", "scala$collection$immutable$RedBlackTree$$join2", "(Lscala/collection/immutable/RedBlackTree$Tree;Lscala/collection/immutable/RedBlackTree$Tree;)Lscala/collection/immutable/RedBlackTree$Tree;")
      .renameTo("scala$collection$immutable$RedBlackTree$$$join2")
      .onOpCode(Opcodes.INVOKEVIRTUAL)
      .applyTo("scala/collection/immutable/RedBlackTree$partitioner$1$.class"),

    // Lambda implementations in Ordering with renamed inner methods
    // Unlikely: requires inlining in users code, can reproduce
    MemberReference("scala/math/Ordering", "scala$math$Ordering$$$anonfun$orElse$1", "(Ljava/lang/Object;Ljava/lang/Object;Lscala/math/Ordering;)I")
      .delegateTo(MemberReference("scala/math/Ordering", "scala$math$Ordering$$_$orElse$$anonfun$1", "(Lscala/math/Ordering;Ljava/lang/Object;Ljava/lang/Object;)I"), Opcodes.INVOKEINTERFACE)
      .onOpCode(Opcodes.INVOKEINTERFACE)
      .applyTo("scala/math/Ordering$$anonfun$orElse$2.class"),
    MemberReference("scala/math/Ordering", "scala$math$Ordering$$$anonfun$orElseBy$1", "(Ljava/lang/Object;Ljava/lang/Object;Lscala/math/Ordering;Lscala/Function1;)I")
      .delegateTo(MemberReference("scala/math/Ordering", "scala$math$Ordering$$_$orElseBy$$anonfun$1", "(Lscala/math/Ordering;Lscala/Function1;Ljava/lang/Object;Ljava/lang/Object;)I"), Opcodes.INVOKEINTERFACE)
      .onOpCode(Opcodes.INVOKEINTERFACE)
      .applyTo("scala/math/Ordering$$anonfun$orElseBy$2.class"),

    // outer field manging didference $$$outer vs $$$$outer
    // Unlikely: requires inlining in users code, can reproduce
    MemberReference("scala/Enumeration$ValueSet$", "scala$Enumeration$ValueSet$$$outer", "()Lscala/Enumeration;")
      .renameTo("scala$Enumeration$ValueSet$$$$outer")
      .onOpCode(Opcodes.INVOKEVIRTUAL)
      .applyTo("scala/Enumeration$ValueSet$$anon$1.class"),

     // Private methods mangling change in Scala 3: add one more $ to make triple $$
     // Unlikely, requires inlining, cannot reproduce
    MemberReference("scala/util/control/Exception$", "scala$util$control$Exception$$wouldMatch", "(Ljava/lang/Throwable;Lscala/collection/Seq;)Z")
      .renameTo("scala$util$control$Exception$$$wouldMatch")
      .onOpCode(Opcodes.INVOKEVIRTUAL)
      .applyTo("scala/util/control/Exception$$anonfun$pfFromExceptions$1.class"),

    // outer fields mangling difference $$anon$$$outer vs $_$$anon$$$outer
    // Unlikely: requies inlining in users code, cannot reproduce
    MemberReference("scala/collection/LazyZip2$$anon$7", "scala$collection$LazyZip2$$anon$$$outer", "()Lscala/collection/LazyZip2;")
      .renameTo("scala$collection$LazyZip2$_$$anon$$$outer")
      .onOpCode(Opcodes.INVOKEVIRTUAL)
      .applyTo("scala/collection/LazyZip2$$anon$7$$anon$8.class"),
    MemberReference("scala/collection/LazyZip3$$anon$15", "scala$collection$LazyZip3$$anon$$$outer", "()Lscala/collection/LazyZip3;")
      .renameTo("scala$collection$LazyZip3$_$$anon$$$outer")
      .onOpCode(Opcodes.INVOKEVIRTUAL)
      .applyTo("scala/collection/LazyZip3$$anon$15$$anon$16.class"),
    MemberReference("scala/collection/LazyZip4$$anon$23", "scala$collection$LazyZip4$$anon$$$outer", "()Lscala/collection/LazyZip4;")
      .renameTo("scala$collection$LazyZip4$_$$anon$$$outer")
      .onOpCode(Opcodes.INVOKEVIRTUAL)
      .applyTo("scala/collection/LazyZip4$$anon$23$$anon$24.class"),
  )

  /** Represents a reference to member in a bytecode  */
  case class MemberReference(owner: String, name: String, descriptor: String)

  /** A rule defintion that might to applied to references given signature
   *  @param appliesTo - list of class fieles to which given rule can be applies
   *  @param appliedToOpCodes - list of op codes for which rewrite is allowed
   */
  case class Rule(
    reference: MemberReference,
    rewrite: RewriteOperation,
    appliesTo: Set[String] = Set.empty,
    appliedToOpCodes: Set[Int] = Set.empty,
  ) {
    def applyTo(classFiles: String*): Rule = copy(appliesTo = this.appliesTo ++ classFiles)
    def onOpCode(opCodes: Int*) = copy(appliedToOpCodes = appliedToOpCodes ++ opCodes)
  }

  implicit class MemberToRewriteOps(val member: MemberReference) extends AnyVal {
    def renameTo(newName: String): Rule =
      Rule(member, RewriteOperation.Rename(newName))

    def delegateTo(target: MemberReference, opCode: Int) =
      Rule(member, RewriteOperation.Delegate(target, opCode = opCode))
    /** Delegates the call to accessor available under given name. Uses implicit descriptor created from member fields
     */
    def delegateToAccessor(name: String): Rule =
      Rule(member, RewriteOperation.Delegate(
        member.copy(
          name = name,
          descriptor = s"()${member.descriptor}".ensuring(!member.descriptor.startsWith("("), s"member is not a field, cannot delegate to accessor of function: ${member}")
        ),
        opCode = Opcodes.INVOKEVIRTUAL
      ))
  }

  sealed trait RewriteOperation
  object RewriteOperation {
    case class Rename(name: String) extends RewriteOperation
    case class Delegate(target: MemberReference, opCode: Int) extends RewriteOperation
  }

  /** Visitor for rewriting field and method references in bytecode */
  class ReferenceRewritingMethodVisitor(
    mv: MethodVisitor,
    classFile: String,
    log: Logger
  ) extends MethodVisitor(Opcodes.ASM9, mv) {
    val rewriteRules = rules.filter(_.appliesTo.contains(classFile))

    if (rewriteRules.nonEmpty){
      val ambigiousReferenceRules = rewriteRules.groupBy(_.reference).filter(_._2.size > 1)
      ambigiousReferenceRules.toSeq.sortBy(_._1.toString()).foreach{
        case (ref, rules) => log.error(s" - $ref: [${rules.size}] ${rules}")
      }
      assert(ambigiousReferenceRules.isEmpty, s"More then one rewrite rule found for rewrites in ${classFile}")
    }

    private val ruleLookup = rewriteRules.map(rule => rule.reference -> rule).toMap

    override def visitFieldInsn(opcode: Int, owner: String, name: String, descriptor: String): Unit = {
      val key = MemberReference(owner, name, descriptor)
      ruleLookup.get(key) match {
        case Some(rule) if rule.appliedToOpCodes.contains(opcode) =>
          BytecodeRewrites.rulesUsage.mark(rule, classFile)
          rule.rewrite match {
            case RewriteOperation.Rename(newName) =>
              super.visitFieldInsn(opcode, owner, newName, descriptor)
            case RewriteOperation.Delegate(target, targetOpCode) =>
              if (opcode == Opcodes.GETSTATIC && targetOpCode == Opcodes.INVOKEVIRTUAL) {
                super.visitFieldInsn(Opcodes.GETSTATIC, owner, "MODULE$", s"L$owner;")
              }
              super.visitMethodInsn(targetOpCode, target.owner, target.name, target.descriptor, false)
          }
        case maybeRule =>
          maybeRule.foreach{ rule =>
            log.warn(s"Matched rule in $classFile not applicable to opcode:${opcode}: $rule")
          }
          super.visitFieldInsn(opcode, owner, name, descriptor)
      }
    }
    override def visitMethodInsn(opcode: Int, owner: String, name: String, descriptor: String, isInterface: Boolean): Unit = {
      val key = MemberReference(owner, name, descriptor)
      ruleLookup.get(key) match {
        case Some(rule) if rule.appliedToOpCodes.contains(opcode) =>
          BytecodeRewrites.rulesUsage.mark(rule, classFile)
          rule.rewrite match {
            case RewriteOperation.Rename(newName) =>
              super.visitFieldInsn(opcode, owner, newName, descriptor)
            case RewriteOperation.Delegate(target, targetOpCode) =>
              super.visitMethodInsn(targetOpCode, target.owner, target.name, target.descriptor, false)
          }
        case maybeRule =>
          maybeRule.foreach{ rule =>
            log.warn(s"Matched rule in $classFile not applicable to opcode:${opcode}: $rule ")
          }
          super.visitMethodInsn(opcode, owner, name, descriptor, isInterface)
      }
    }
  }

  def validateRewrittenTargetsPublic(classDir: File): Unit = {
    val errors = mutable.ListBuffer.empty[String]
    for ((owner, targets) <- BytecodeRewrites.rewriteTargetsByOwner){
      val classFile = classDir / s"$owner.class"
      if (!classFile.exists()) {
        errors += s"$owner: class file not found at $classFile"
      } else {
        val fields = mutable.Map.empty[(String, String), Int]
        val methods = mutable.Map.empty[(String, String), Int]
        val visitor = new ClassVisitor(Opcodes.ASM9) {
          override def visitField(access: Int, name: String, descriptor: String, signature: String, value: Object): FieldVisitor = {
            fields += (name -> descriptor) -> access
            null
          }
          override def visitMethod(access: Int, name: String, descriptor: String, signature: String, exceptions: Array[String]): MethodVisitor = {
            methods += (name -> descriptor) -> access
            null
          }
        }
        new ClassReader(IO.readBytes(classFile)).accept(
          visitor,
          ClassReader.SKIP_CODE | ClassReader.SKIP_DEBUG | ClassReader.SKIP_FRAMES
        )
        for (target <- targets) {
          val key = target.name -> target.descriptor
          (fields.get(key), methods.get(key)) match {
            case (Some(access), _) =>
              if ((access & Opcodes.ACC_PUBLIC) == 0) {
                errors += s"$owner.${target.name}${target.descriptor}: field is not public"
              }
            case (None, Some(access)) =>
              if ((access & Opcodes.ACC_PUBLIC) == 0) {
                errors += s"$owner.${target.name}${target.descriptor}: method is not public"
              }
            case (None, None) =>
              errors += s"$owner.${target.name}${target.descriptor}: member not found"
          }
        }
      }
    }
    assert(
      errors.isEmpty,
      s"Rewritten-to members must be public, but ${errors.size} issues found:\n  - ${errors.mkString("\n  - ")}"
    )
  }
}