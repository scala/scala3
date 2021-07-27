package dotty.tools.scripting

import java.nio.file.{ Files, Paths, Path }
import java.io.File
import java.net.{ URL, URLClassLoader }
import java.lang.reflect.{ Modifier, Method }

import scala.jdk.CollectionConverters._

import dotty.tools.dotc.{ Driver, Compiler }
import dotty.tools.dotc.core.Contexts, Contexts.{ Context, ContextBase, ctx }
import dotty.tools.dotc.config.CompilerCommand
import dotty.tools.io.{ PlainDirectory, Directory, ClassPath }
import dotty.tools.dotc.reporting.Reporter
import dotty.tools.dotc.config.Settings.Setting._

import sys.process._

class ScriptingDriver(compilerArgs: Array[String], scriptFile: File, scriptArgs: Array[String]) extends Driver:
  def compileAndRun(pack:(Path, Seq[Path], String) => Boolean = null): Unit =
    val outDir = Files.createTempDirectory("scala3-scripting")
    outDir.toFile.deleteOnExit()
    setup(compilerArgs :+ scriptFile.getAbsolutePath, initCtx.fresh) match
      case Some((toCompile, rootCtx)) =>
        given Context = rootCtx.fresh.setSetting(rootCtx.settings.outputDir,
          new PlainDirectory(Directory(outDir)))

        if doCompile(newCompiler, toCompile).hasErrors then
          throw ScriptingException("Errors encountered during compilation")

        try
          val classpath = s"${ctx.settings.classpath.value}${pathsep}${sys.props("java.class.path")}"
          val classpathEntries: Seq[Path] = ClassPath.expandPath(classpath, expandStar=true).map { Paths.get(_) }
          val (mainClass, mainMethod) = detectMainClassAndMethod(outDir, classpathEntries, scriptFile)
          val invokeMain: Boolean =
            Option(pack) match
              case Some(func) =>
                func(outDir, classpathEntries, mainClass)
              case None =>
                true
            end match
          if invokeMain then mainMethod.invoke(null, scriptArgs)
        catch
          case e: java.lang.reflect.InvocationTargetException =>
            throw e.getCause
        finally
          deleteFile(outDir.toFile)
      case None =>
  end compileAndRun

  private def deleteFile(target: File): Unit =
    if target.isDirectory then
      for member <- target.listFiles.toList
      do deleteFile(member)
    target.delete()
  end deleteFile

  private def detectMainClassAndMethod(outDir: Path, classpathEntries: Seq[Path],
      scriptFile: File): (String, Method) =

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

    val candidates = for
      file <- outDir.toFile.listFiles.toList
      method <- collectMainMethods(file, "")
    yield method

    candidates match
      case Nil =>
        throw ScriptingException(s"No main methods detected in script ${scriptFile}")
      case _ :: _ :: _ =>
        throw ScriptingException("A script must contain only one main method. " +
          s"Detected the following main methods:\n${candidates.mkString("\n")}")
      case m :: Nil => m
    end match
  end detectMainClassAndMethod

  def pathsep = sys.props("path.separator")

end ScriptingDriver

case class ScriptingException(msg: String) extends RuntimeException(msg)
