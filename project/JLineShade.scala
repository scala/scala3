import java.io.{ByteArrayOutputStream, File, InputStream}
import java.util.jar.{JarEntry, JarFile, JarOutputStream}
import java.nio.file.Files

import org.objectweb.asm.{ClassReader, ClassWriter}
import org.objectweb.asm.commons.{ClassRemapper, Remapper}

/** Helpers for the JLine shading pipeline used by `jline-shaded`. */
object JLineShade {

  private val ShadedPrefix = "dotty.shaded.org.jline."
  private val UnshadedPrefix = "org.jline."

  /**
   * JarJar rename rules also rewrite string constants that *look* like class
   * names. That corrupts JLine's system-property keys (e.g.
   * `org.jline.terminal.provider`). Restore any shaded dotted string that does
   * not correspond to a class actually present in the jar.
   */
  def restoreNonClassStringConstants(inJar: File, outJar: File): Unit = {
    val classNames = collectDottedClassNames(inJar)
    val remapper = new Remapper {
      override def map(internalName: String): String = internalName
      override def mapValue(value: AnyRef): AnyRef = value match {
        case s: String => restoreIfNotAClass(s, classNames)
        case other     => other
      }
    }

    val buffer = new ByteArrayOutputStream(Math.max(32 * 1024, inJar.length().toInt))
    val jos = new JarOutputStream(buffer)
    val jf = new JarFile(inJar)
    try {
      val entries = jf.entries()
      while (entries.hasMoreElements) {
        val entry = entries.nextElement()
        val bytes = readAll(jf.getInputStream(entry))
        val outBytes =
          if (entry.getName.endsWith(".class") && !entry.isDirectory)
            rewriteClass(bytes, remapper)
          else bytes
        val outEntry = new JarEntry(entry.getName)
        outEntry.setTime(entry.getTime)
        jos.putNextEntry(outEntry)
        jos.write(outBytes)
        jos.closeEntry()
      }
    } finally {
      jf.close()
      jos.close()
    }
    Files.write(outJar.toPath, buffer.toByteArray)
  }

  private def collectDottedClassNames(jar: File): Set[String] = {
    val jf = new JarFile(jar)
    try {
      val b = Set.newBuilder[String]
      val entries = jf.entries()
      while (entries.hasMoreElements) {
        val name = entries.nextElement().getName
        if (name.endsWith(".class") && !name.contains("module-info"))
          b += name.stripSuffix(".class").replace('/', '.')
      }
      b.result()
    } finally jf.close()
  }

  private def restoreIfNotAClass(s: String, classNames: Set[String]): String =
    if (!s.startsWith(ShadedPrefix)) s
    else if (isPresentClass(s, classNames)) s
    else UnshadedPrefix + s.substring(ShadedPrefix.length)

  /** True if `dotted` names a class (or outer class of an inner class) in the jar. */
  private def isPresentClass(dotted: String, classNames: Set[String]): Boolean =
    classNames.contains(dotted) ||
      classNames.exists(c => c.startsWith(dotted + "$$") || c.startsWith(dotted + "$"))

  private def rewriteClass(bytes: Array[Byte], remapper: Remapper): Array[Byte] = {
    val cr = new ClassReader(bytes)
    val cw = new ClassWriter(0)
    cr.accept(new ClassRemapper(cw, remapper), 0)
    cw.toByteArray
  }

  private def readAll(in: InputStream): Array[Byte] = {
    try {
      val buf = new ByteArrayOutputStream()
      val tmp = new Array[Byte](8192)
      Iterator
        .continually(in.read(tmp))
        .takeWhile(_ != -1)
        .foreach(n => buf.write(tmp, 0, n))
      buf.toByteArray
    } finally in.close()
  }
}
