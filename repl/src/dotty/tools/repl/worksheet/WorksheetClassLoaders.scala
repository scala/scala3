package dotty.tools.repl.worksheet

import java.io.Closeable

private object WorksheetClassLoaders:
  def closeCreated(innermost: ClassLoader): Unit =
    created(innermost).foreach:
      case closeable: Closeable => closeable.close()
      case _ => ()

  private def created(loader: ClassLoader | Null): List[ClassLoader] =
    loader match
      case null => Nil
      case loader if isPreexisting(loader) => Nil
      case loader => loader :: created(loader.getParent)

  private def isPreexisting(loader: ClassLoader): Boolean =
    val system = ClassLoader.getSystemClassLoader
    loader == getClass.getClassLoader || loader == system || loader == system.getParent
