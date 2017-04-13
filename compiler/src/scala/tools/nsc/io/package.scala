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
  type AbstractFile = scala.reflect.io.AbstractFile
  val AbstractFile = scala.reflect.io.AbstractFile

  type Directory = scala.reflect.io.Directory
  val Directory = scala.reflect.io.Directory

  type Path = scala.reflect.io.Path
  val Path = scala.reflect.io.Path

  type File = scala.reflect.io.File
  val File = scala.reflect.io.File

  type Jar = dotty.tools.io.Jar
  val Jar = dotty.tools.io.Jar
}
