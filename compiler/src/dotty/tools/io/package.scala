/* NSC -- new Scala compiler
 * Copyright 2005-2012 LAMP/EPFL
 * @author Paul Phillips
 */

package dotty.tools

import java.util.concurrent.{ Future, Callable }
import java.util.{ Timer, TimerTask }
import java.util.jar.{ Attributes }
import scala.language.implicitConversions

package object io {
  // Forwarders from scala.reflect.io
  type AbstractFile = scala.reflect.io.AbstractFile
  val AbstractFile = scala.reflect.io.AbstractFile
  type Directory = scala.reflect.io.Directory
  val Directory = scala.reflect.io.Directory
  type File = scala.reflect.io.File
  val File = scala.reflect.io.File
  type ManifestResources = scala.reflect.io.ManifestResources
  type Path = scala.reflect.io.Path
  val Path = scala.reflect.io.Path
  type VirtualDirectory = scala.reflect.io.VirtualDirectory
  type VirtualFile = scala.reflect.io.VirtualFile
  type ZipArchive = scala.reflect.io.ZipArchive
  type FileZipArchive = scala.reflect.io.FileZipArchive
  type JManifest = java.util.jar.Manifest
  type JFile = java.io.File
}
