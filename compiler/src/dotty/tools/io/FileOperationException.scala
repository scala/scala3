/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools.io
/** ''Note:  This library is considered experimental and should not be used unless you know what you are doing.'' */
case class FileOperationException(msg: String) extends RuntimeException(msg)
