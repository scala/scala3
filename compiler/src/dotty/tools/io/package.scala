/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author Paul Phillips
 */

package dotty.tools

package object io {
  type JManifest = java.util.jar.Manifest
  type JFile = java.io.File
  type JPath = java.nio.file.Path
}
