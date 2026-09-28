package dotty.tools.scripting

import java.net.URLClassLoader
import java.lang.reflect.{ Modifier, Method }
import dotty.tools.nio.*
import dotty.tools.dotc.classpath.ClassPath

object Util:
  def detectMainClassAndMethod(
    outDir: FileContainer,
    classpathEntries: Seq[FileSystemEntry],
    srcFile: String
  ): Either[Throwable, (String, Method)] =
    val classpathUrls = (classpathEntries :+ outDir).flatMap(_.toURL)
    val cl = URLClassLoader(classpathUrls.toArray)

    def collectMainMethods(entry: FileSystemEntry, path: String): List[(String, Method)] = entry match {
      case c: FileContainer =>
        val targetPath =
          if path.nonEmpty then s"$path.${c.name}"
          else c.name
        for
          packageMember <- c.entries.toList
          membersMainMethod <- collectMainMethods(packageMember, targetPath)
        yield membersMainMethod
      case f: File if f.extension.isClass =>
        val targetPath =
          if path.nonEmpty then s"$path.${f.nameWithoutExtension}"
          else f.nameWithoutExtension
        val cls = cl.loadClass(targetPath)
        try
          val method = cls.getMethod("main", classOf[Array[String]])
          if Modifier.isStatic(method.getModifiers) then List((cls.getName, method)) else Nil
        catch
          case _: java.lang.NoSuchMethodException => Nil
      case _ => Nil
    }

    val mains = for
      entry <- outDir.entries
      method <- collectMainMethods(entry, "")
    yield method

    mains match
      case Nil =>
        Left(StringDriverException(s"No main methods detected for [${srcFile}]"))
      case _ :: _ :: _ =>
        Left(StringDriverException(s"Internal error: Detected the following main methods:\n${mains.mkString("\n")}"))
      case mainMethod :: Nil => Right(mainMethod)
    end match
  end detectMainClassAndMethod

  def pathsep: String = ClassPath.pathSeparator

end Util

