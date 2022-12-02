package dotty.tools.dotc

import dotty.tools.FatalError

class MissingCoreLibraryException(rootPackage: String) extends FatalError(
  s"""Could not find package $rootPackage from compiler core libraries.
     |Make sure the compiler core libraries are on the classpath.
   """.stripMargin
)
