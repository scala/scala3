package dotty.tools.scripting

import scala.language.unsafeNulls

import java.nio.file.{ Path }
import java.io.File
import java.net.{ URLClassLoader }
import java.lang.reflect.{ Modifier, Method }

object Util:

  def deleteFile(target: File): Unit =
    if target.isDirectory then
      for member <- target.listFiles.toList
      do deleteFile(member)
    target.delete()
  end deleteFile

  def detectMainClassAndMethod(outDir: Path, classpathEntries: Seq[Path], srcFile: String): (String, Method) =
    val classpathUrls = (classpathEntries :+ outDir).map { _.toUri.toURL }
    val cl = URLClassLoader(classpathUrls.toArray)

    def collectMainMethods(target: File, path: String): List[(String, Method)] =
      val nameWithoutExtension = target.getName.takeWhile(_ != '.')
      val targetPath =
        if path.nonEmpty then s"${path}.${nameWithoutExtension}"
        else nameWithoutExtension

      if target.isDirectory then
        for
          packageMember <- target.listFiles.toList
          membersMainMethod <- collectMainMethods(packageMember, targetPath)
        yield membersMainMethod
      else if target.getName.endsWith(".class") then
        val cls = cl.loadClass(targetPath)
        try
          val method = cls.getMethod("main", classOf[Array[String]])
          if Modifier.isStatic(method.getModifiers) then List((cls.getName, method)) else Nil
        catch
          case _: java.lang.NoSuchMethodException => Nil
      else Nil
    end collectMainMethods

    val mains = for
      file <- outDir.toFile.listFiles.toList
      method <- collectMainMethods(file, "")
    yield method

    mains match
      case Nil =>
        throw StringDriverException(s"No main methods detected for [${srcFile}]")
      case _ :: _ :: _ =>
        throw StringDriverException(
          s"internal error: Detected the following main methods:\n${mains.mkString("\n")}")
      case m :: Nil => m
    end match
  end detectMainClassAndMethod

  def pathsep = sys.props("path.separator")

end Util

