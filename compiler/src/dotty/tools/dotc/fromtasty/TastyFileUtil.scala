package dotty.tools.dotc
package fromtasty

import dotty.tools.dotc.core.tasty.TastyClassName
import dotty.tools.dotc.core.StdNames.nme.EMPTY_PACKAGE
import dotty.tools.nio.*

object TastyFileUtil {
  /** Get the class path of a tasty file
   *
   *  If
   *  ```scala
   *    package foo
   *    class Foo
   *  ```
   *  then `getClassPath("./out/foo/Foo.tasty") returns `Some("./out")`
   */
  def getClassPath(file: File, fromBestEffortTasty: Boolean = false): Option[String] =
    getClassName(file, fromBestEffortTasty).map { className =>
      val extension = if (fromBestEffortTasty) then ".betasty" else ".tasty"
      val classInPath = className.replace('.', FileSystemEntry.separator) + extension
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
  def getClassName(file: File, withBestEffortTasty: Boolean = false): Option[String] =
    assert(file.extension.isTasty || (withBestEffortTasty && file.extension.isBetasty))
    val bytes = file.readBytes()
    val names = new TastyClassName(bytes, file.extension.isBetasty).readName()
    names.map: (packageName, className) =>
      if packageName == EMPTY_PACKAGE then
        s"${className.lastPart.encode}"
      else
        s"${packageName.encode}.${className.lastPart.encode}"
}


