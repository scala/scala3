package dotty.tools.sbtplugin

import org.objectweb.asm.*

import java.io.{BufferedInputStream, File}
import java.util.jar.JarFile

import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Using

object ScalaJarValidate {
  import StripScala2Annotations.{
    Scala2PickleAttributes,
    extractSourceFile,
    extractTastyUUID,
    extractTastyUUIDFromClass,
    hasScalaAttribute,
    isScala2PickleAnnotation
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

  /** Validate that all files produced by Scala compiler have a "Scala" attribute.
   *  Java-sourced files are excluded from this check since they don't have Scala attributes.
   */
  def validateScalaAttributes(jar: File): Unit = {
    val classFilesWithoutScala = Using.resource(new JarFile(jar, true)) { jarFile =>
      jarFile
        .entries().asScala
        .filter(_.getName.endsWith(".class"))
        .flatMap { entry =>
          Using.resource(new BufferedInputStream(jarFile.getInputStream(entry))) { inputStream =>
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
    val classFilesWithPickles = Using.resource(new JarFile(jar, true)) { jarFile =>
      jarFile
      .entries().asScala
      .filter(_.getName.endsWith(".class"))
      .flatMap { entry =>
        Using.resource(new BufferedInputStream(jarFile.getInputStream(entry))){ inputStream =>
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

  /** Validate TASTY attributes in the JAR:
   *  - If a .class file has a TASTY attribute, verify its UUID matches a .tasty file in the JAR
   *  - Every .tasty file must have at least one .class file with a matching TASTY attribute
   *
   *  Note: .class files from Java sources don't have .tasty files and are allowed to not have TASTY attributes.
   */
  def validateTastyAttributes(jar: File): Unit = {
    Using.resource(new JarFile(jar, true)) { jarFile =>
      // Build a map of .tasty file paths to their UUIDs
      val tastyEntries = jarFile.entries().asScala
        .filter(_.getName.endsWith(".tasty"))
        .map { entry =>
          val bytes = Using.resource(new BufferedInputStream(jarFile.getInputStream(entry)))(_.readAllBytes())
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
          val classBytes = Using.resource(new BufferedInputStream(jarFile.getInputStream(entry)))(_.readAllBytes())
          val classPath = entry.getName

          // Only validate classes that have a TASTY attribute
          extractTastyUUIDFromClass(classBytes).foreach[Unit] { classUUID =>
            // Find a .tasty file with matching UUID
            tastyEntries.find{ case (path, tastyUUID) =>
            java.util.Arrays.equals(classUUID, tastyUUID) && {
              val tastyName = new File(path).getName().stripSuffix(".tasty")
              val className = new File(entry.getName()).getName().stripSuffix(".class")
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
}
