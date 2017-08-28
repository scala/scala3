package dotty.tools
package repl

import io.AbstractFile

/**
 * A class loader that loads files from a {@link scala.tools.nsc.io.AbstractFile}.
 *
 * @author Lex Spoon
 */
class AbstractFileClassLoader(root: AbstractFile, parent: ClassLoader)
extends ClassLoader(parent)
{
  override def findClass(name: String): Class[_] = {
    var file: AbstractFile = root
    val pathParts = name.split("[./]").toList
    for (dirPart <- pathParts.init) {
      file = file.lookupName(dirPart, true)
      if (file == null) {
        throw new ClassNotFoundException(name)
      }
    }
    file = file.lookupName(pathParts.last+".class", false)
    if (file == null) {
      throw new ClassNotFoundException(name)
    }
    val bytes = file.toByteArray
    defineClass(name, bytes, 0, bytes.length)
  }

  override def loadClass(name: String): Class[_] =
    try findClass(name)
    catch {
      case _: ClassNotFoundException => super.loadClass(name)
    }
}
