package dotty.tools.scripting

import dotty.tools.dotc.Driver
import dotty.tools.dotc.core.Contexts, Contexts.{ Context, ctx }
import dotty.tools.nio.*
import dotty.tools.dotc.classpath.ClassPath
import Util.*

class ScriptingDriver(compilerArgs: Array[String], scriptFile: File, scriptArgs: Array[String]) extends Driver:
  def compileAndRun(pack: (Context ?=> (FileContainer, Seq[FileContainer], String) => Boolean) | Null = null): Option[Throwable] =
    val outDir = FileContainer.createTemporaryOnDisk("scala3-scripting")
    setup(compilerArgs :+ scriptFile.path, initCtx.fresh) match
      case Some((toCompile, rootCtx)) =>
        given Context = rootCtx.fresh.setSetting(rootCtx.settings.outputDir, outDir)

        if doCompile(newCompiler, toCompile).hasErrors then
          Some(ScriptingException("Errors encountered during compilation"))
        else
          try
            val classpath = s"${ctx.settings.classpath.value}${pathsep}${sys.props("java.class.path")}"
            val classpathEntries = ClassPath.expandPath(classpath).map(FileContainer.getOrCreateOnDisk)
            detectMainClassAndMethod(outDir, classpathEntries, scriptFile.path) match
              case Right((mainClass, mainMethod)) =>
                val invokeMain: Boolean = Option(pack).forall { func =>
                  func(outDir, classpathEntries, mainClass)
                }
                if invokeMain then mainMethod.invoke(null, scriptArgs)
                None
              case Left(ex) => Some(ex)
          catch
            case e: java.lang.reflect.InvocationTargetException =>
              Some(e.getCause)
          finally
            outDir.deleteRecursively()
      case None => None
  end compileAndRun

end ScriptingDriver

case class ScriptingException(msg: String) extends RuntimeException(msg)
