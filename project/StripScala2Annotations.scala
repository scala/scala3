package dotty.tools.sbtplugin

import dotty.tools.tasty.TastyHeaderUnpickler
import org.objectweb.asm.*

import java.nio.ByteBuffer

import sbt._

object StripScala2Annotations {
  /** Scala 2 pickle annotation descriptors that should be stripped from class files */
  private val Scala2PickleAnnotations = Set(
    "Lscala/reflect/ScalaSignature;",
    "Lscala/reflect/ScalaLongSignature;"
  )

  /** Scala 2 attribute names that should be stripped from class files */
  val Scala2PickleAttributes = Set("ScalaSig", "ScalaInlineInfo")

  /** Check if an annotation descriptor is a Scala 2 pickle annotation */
  def isScala2PickleAnnotation(descriptor: String): Boolean =
    Scala2PickleAnnotations.contains(descriptor)

  /** Extract the UUID bytes (16 bytes) from a TASTy file.
   *
   *  Uses the official TastyHeaderUnpickler to parse the header and extract the UUID,
   *  ensuring correctness and validating the TASTy format.
   */
  def extractTastyUUID(tastyBytes: Array[Byte]): Array[Byte] = {
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
  def extractTastyUUIDFromClass(bytes: Array[Byte]): Option[Array[Byte]] = {
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

  /** Extract the SourceFile attribute from class file bytecode */
  def extractSourceFile(bytes: Array[Byte]): Option[String] = {
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

  /** Check if class file bytecode contains a Scala attribute */
  def hasScalaAttribute(bytes: Array[Byte]): Boolean = {
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
