/*
 * Dotty (https://dotty.epfl.ch/)
 *
 * Copyright EPFL.
 *
 * Licensed under Apache License 2.0
 * (https://www.apache.org/licenses/LICENSE-2.0).
 */

package dotty.tools

package object io {
  type JManifest = java.util.jar.Manifest
  type JFile = java.io.File
  type JPath = java.nio.file.Path
}
