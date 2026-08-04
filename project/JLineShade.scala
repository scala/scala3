import java.io.{ByteArrayOutputStream, File, InputStream}
import java.util.jar.{JarEntry, JarFile, JarOutputStream}
import java.nio.file.Files

import org.objectweb.asm.{ClassReader, ClassWriter, Opcodes}
import org.objectweb.asm.commons.{ClassRemapper, Remapper}

/** Helpers for the JLine shading pipeline used by `scala3-jline-shaded`. */
object JLineShade {

  private val ShadedPrefix = "dotty.shaded.org.jline."
  private val UnshadedPrefix = "org.jline."

  /**
   * Module name must stay `org.jline.terminal`: Scala CLI hardcodes
   * `--add-modules org.jline.terminal` for JDK 24+ (see VirtusLab/scala-cli
   * `ReplArtifacts`). The jar still exports `dotty.shaded.org.jline.**`.
   */
  val ModuleName = "org.jline.terminal"

  private val TerminalProvider =
    "dotty.shaded.org.jline.terminal.spi.TerminalProvider"
  private val JniTerminalProvider =
    "dotty.shaded.org.jline.terminal.impl.jni.JniTerminalProvider"
  private val TerminalGraphics =
    "dotty.shaded.org.jline.terminal.impl.TerminalGraphics"

  /**
   * JarJar rename rules also rewrite string constants that *look* like class
   * names. That corrupts JLine's system-property keys (e.g.
   * `org.jline.terminal.provider`). Restore any shaded dotted string that does
   * not correspond to a class actually present in the jar, then inject a
   * `module-info.class`.
   */
  def finalizeShadedJar(inJar: File, outJar: File): Unit = {
    val classNames = collectDottedClassNames(inJar)
    requireClass(classNames, TerminalProvider)
    requireClass(classNames, JniTerminalProvider)
    requireClass(classNames, TerminalGraphics)

    val remapper = new Remapper {
      override def map(internalName: String): String = internalName
      override def mapValue(value: AnyRef): AnyRef = value match {
        case s: String => restoreIfNotAClass(s, classNames)
        case other     => other
      }
    }

    val packages = classNames.flatMap(packageOf)
    val moduleInfo = moduleInfoBytes(packages)

    val buffer = new ByteArrayOutputStream(Math.max(32 * 1024, inJar.length().toInt))
    val jos = new JarOutputStream(buffer)
    val jf = new JarFile(inJar)
    try {
      val entries = jf.entries()
      while (entries.hasMoreElements) {
        val entry = entries.nextElement()
        // Discard any upstream / stale module descriptor; we inject our own.
        if (entry.getName != "module-info.class") {
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
      }
      val mi = new JarEntry("module-info.class")
      jos.putNextEntry(mi)
      jos.write(moduleInfo)
      jos.closeEntry()
    } finally {
      jf.close()
      jos.close()
    }
    Files.write(outJar.toPath, buffer.toByteArray)
  }

  /**
   * Build `module-info.class` for an open module named [[ModuleName]].
   * Exports every package found in the shaded jar; requires `org.jline.nativ`
   * transitively so `--enable-native-access=org.jline.nativ` names a resolved
   * module when Scala CLI puts both jars on the module path.
   */
  def moduleInfoBytes(packages: Set[String]): Array[Byte] = {
    val cw = new ClassWriter(0)
    cw.visit(
      Opcodes.V9,
      Opcodes.ACC_MODULE,
      "module-info",
      null,
      null,
      null,
    )
    val mv = cw.visitModule(ModuleName, Opcodes.ACC_OPEN, null)
    mv.visitRequire("java.base", Opcodes.ACC_MANDATED, null)
    mv.visitRequire("org.jline.nativ", Opcodes.ACC_TRANSITIVE, null)
    mv.visitRequire("java.desktop", Opcodes.ACC_STATIC_PHASE, null)
    packages.toSeq.sorted.foreach(pkg => mv.visitExport(pkg.replace('.', '/'), 0))
    mv.visitUse(TerminalProvider.replace('.', '/'))
    mv.visitUse(TerminalGraphics.replace('.', '/'))
    mv.visitProvide(
      TerminalProvider.replace('.', '/'),
      JniTerminalProvider.replace('.', '/'),
    )
    mv.visitEnd()
    cw.visitEnd()
    cw.toByteArray
  }

  private def requireClass(classNames: Set[String], dotted: String): Unit =
    if (!classNames.contains(dotted))
      throw new IllegalStateException(
        s"""shaded JLine jar is missing required class $dotted;
           |a JLine upgrade may have renamed it""".stripMargin
      )

  private def packageOf(dottedClass: String): Option[String] = {
    val i = dottedClass.lastIndexOf('.')
    if (i <= 0) None
    else Some(dottedClass.substring(0, i))
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

  private def readAll(in: InputStream): Array[Byte] =
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
