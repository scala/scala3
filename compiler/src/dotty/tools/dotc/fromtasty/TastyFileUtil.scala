package dotty.tools.dotc
package fromtasty

import scala.language.unsafeNulls

import dotty.tools.dotc.core.tasty.TastyClassName
import dotty.tools.dotc.core.StdNames.nme.EMPTY_PACKAGE
import dotty.tools.io.AbstractFile
import dotty.tools.dotc.classpath.FileUtils.hasTastyExtension
import dotty.tools.dotc.classpath.FileUtils.hasBetastyExtension

object TastyFileUtil {
  /** Get the class path of a tasty file
   *
   *  If
   *  ```scala
   *    package foo
   *    class Foo
   *  ```
   *  then `getClassName("./out/foo/Foo.tasty") returns `Some("./out")`
   */
  def getClassPath(file: AbstractFile, fromBestEffortTasty: Boolean = false): Option[String] =
    getClassName(file, fromBestEffortTasty).map { className =>
      val extension = if (fromBestEffortTasty) then ".betasty" else ".tasty"
      val classInPath = className.replace(".", java.io.File.separator) + extension
      file.path.replace(classInPath, "")
    }

  /** Get the class path of a tasty file
   *
   *  If
   *  ```scala
   *    package foo
   *    class Foo
   *  ```
   *  then `getClassName("./out/foo/Foo.tasty") returns `Some("foo.Foo")`
   */
  def getClassName(file: AbstractFile, withBestEffortTasty: Boolean = false): Option[String] = {
    assert(file.exists)
    assert(file.hasTastyExtension || (withBestEffortTasty && file.hasBetastyExtension))
    val bytes = file.toByteArray
    val names = new TastyClassName(bytes, file.hasBetastyExtension).readName()
    names.map { case (packageName, className) =>
      val fullName = packageName match {
        case EMPTY_PACKAGE => s"${className.lastPart}"
        case _ => s"$packageName.${className.lastPart}"
      }
      fullName
    }
  }
}


