package dotty.tools.dotc
package fromtasty

import java.nio.file.{Files, Path, Paths}
import java.io

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.Names.SimpleName
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.tasty.{TastyUnpickler, TreePickler}
import dotty.tools.dotc.core.StdNames.nme.EMPTY_PACKAGE

object TastyFileUtil {

  /** Get the class path and the class name including packages
   *
   *  If
   *  ```scala
   *    package foo
   *    class Foo
   *  ```
   *  then `getClassName("./out/foo/Foo.tasty") returns `Some(("./out", "foo.Foo"))`
   */
  def getClassName(path: Path): Option[(String, String)] = {
    assert(path.toString.endsWith(".tasty"))
    assert(Files.exists(path))
    val bytes = Files.readAllBytes(path)
    val names = new core.tasty.TastyClassName(bytes).readName()
    names.map { case (packageName, className) =>
      val fullName = packageName match {
        case EMPTY_PACKAGE => s"${className.lastPart}"
        case _ => s"$packageName.${className.lastPart}"
      }
      val classInPath = fullName.replace(".", io.File.separator) + ".tasty"
      val classpath = path.toString.replace(classInPath, "")
      (classpath, fullName)
    }
  }
}


