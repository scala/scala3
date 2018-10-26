package dotty.tools.dotc.fromtasty

import java.nio.file.{Files, Path, Paths}

import dotty.tools.dotc.core.Contexts.Context
import dotty.tools.dotc.core.NameKinds
import dotty.tools.dotc.core.Names.SimpleName
import dotty.tools.dotc.core.StdNames.nme
import dotty.tools.dotc.core.tasty.{TastyUnpickler, TreePickler}

object TastyFileUtil {

  /** Get the class path and the class name including packages
   *
   *  If
   *  ```scala
   *    package foo
   *    class Foo
   *  ```
   *  then `getClassName("./out/foo/Foo.tasty") returns `("./out", "foo.Foo")`
   */
  def getClassName(path: Path): (String, String) = {
    assert(path.toString.endsWith(".tasty"))
    assert(Files.exists(path))
    val bytes = Files.readAllBytes(path)
    val unpickler: TastyUnpickler = new TastyUnpickler(bytes)
    val className =
      unpickler.nameAtRef.contents.iterator.dropWhile {
        name => name.toString == TreePickler.sectionName || name == nme.EMPTY_PACKAGE
      }.takeWhile {
        case name: SimpleName => name != nme.CONSTRUCTOR
        case name => !name.is(NameKinds.ModuleClassName)
      }.collect {
        case name: SimpleName => name.toString
      }.toList
    val classInPath = className.mkString("", "/", ".tasty")
    val classpath = path.toString.replace(classInPath, "")
    (classpath, className.mkString("."))
  }

}
