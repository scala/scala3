package dotty.tools.dotc
package fromtasty

import scala.language.unsafeNulls

import dotty.tools.dotc.core.tasty.TastyClassName
import dotty.tools.dotc.core.StdNames.nme.EMPTY_PACKAGE
import dotty.tools.io.AbstractFile

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
  def getClassPath(file: AbstractFile): Option[String] =
    getClassName(file).map { className =>
      val classInPath = className.replace(".", java.io.File.separator) + ".tasty"
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
  def getClassName(file: AbstractFile): Option[String] = {
    assert(file.exists)
    assert(file.extension == "tasty")
    val bytes = file.toByteArray
    val names = new TastyClassName(bytes).readName()
    names.map: (packageName, className) =>
      if packageName == EMPTY_PACKAGE then
        s"${className.lastPart.encode}"
      else
        s"${packageName.encode}.${className.lastPart.encode}"
}


