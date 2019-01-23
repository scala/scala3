/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools.dotc

import dotty.tools.FatalError

class MissingCoreLibraryException(rootPackage: String) extends FatalError(
  s"""Could not find package $rootPackage from compiler core libraries.
     |Make sure the compiler core libraries are on the classpath.
   """.stripMargin
)
