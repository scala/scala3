package scala.tools.nsc

/**
 * Compatibility layer needed for the backend.
 *
 * Our backend is based on the Scala 2.11 GenBCode backend and modified so that
 * it compiles both with dotty and scalac, since the backend uses
 * scala.tools.nsc.io.*, we need to also provide it.
 *
 * See http://dotty.epfl.ch/docs/contributing/backend.html for more information.
 */
package object io {
  type AbstractFile = dotty.tools.io.AbstractFile
  val AbstractFile = dotty.tools.io.AbstractFile

  type Directory = dotty.tools.io.Directory
  val Directory = dotty.tools.io.Directory

  type Path = dotty.tools.io.Path
  val Path = dotty.tools.io.Path

  type File = dotty.tools.io.File
  val File = dotty.tools.io.File

  type Jar = dotty.tools.io.Jar
  val Jar = dotty.tools.io.Jar
}
