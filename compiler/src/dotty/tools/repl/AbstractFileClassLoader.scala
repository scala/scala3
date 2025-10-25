/*
 * Scala (https://www.scala-lang.org)
 *
 * Copyright EPFL and Lightbend, Inc.
 *
 * Licensed under Apache License 2.0
 * (http://www.apache.org/licenses/LICENSE-2.0).
 *
 * See the NOTICE file distributed with this work for
 * additional information regarding copyright ownership.
 */

package dotty.tools
package repl

import scala.language.unsafeNulls

import io.AbstractFile
import dotty.tools.repl.ReplBytecodeInstrumentation

import java.net.{URL, URLConnection, URLStreamHandler}
import java.util.Collections

class AbstractFileClassLoader(val root: AbstractFile, parent: ClassLoader, instrumentBytecode: Boolean) extends ClassLoader(parent):
  private def findAbstractFile(name: String) = root.lookupPath(name.split('/').toIndexedSeq, directory = false)

  // on JDK 20 the URL constructor we're using is deprecated,
  // but the recommended replacement, URL.of, doesn't exist on JDK 8
  @annotation.nowarn("cat=deprecation")
  override protected def findResource(name: String): URL | Null =
    findAbstractFile(name) match
      case null => null
      case file => new URL(null, s"memory:${file.path}", new URLStreamHandler {
        override def openConnection(url: URL): URLConnection = new URLConnection(url) {
          override def connect() = ()
          override def getInputStream = file.input
        }
      })
  override protected def findResources(name: String): java.util.Enumeration[URL] =
    findResource(name) match
      case null => Collections.enumeration(Collections.emptyList[URL])  //Collections.emptyEnumeration[URL]
      case url  => Collections.enumeration(Collections.singleton(url))

  override def findClass(name: String): Class[?] = {
    var file: AbstractFile | Null = root
    val pathParts = name.split("[./]").toList
    for (dirPart <- pathParts.init) {
      file = file.lookupName(dirPart, true)
      if (file == null) {
        throw new ClassNotFoundException(name)
      }
    }
    file = file.lookupName(pathParts.last+".class", false)
    if (file == null) {
      throw new ClassNotFoundException(name)
    }
    val originalBytes = file.toByteArray

    // Instrument bytecode for everything except StopRepl itself to avoid infinite recursion
    val bytes =
      if !instrumentBytecode || name == "dotty.tools.repl.StopRepl" then originalBytes
      else ReplBytecodeInstrumentation.instrument(originalBytes)

    defineClass(name, bytes, 0, bytes.length)
  }

  private def tryInstrumentLibraryClass(name: String): Class[?] =
    try
      val resourceName = name.replace('.', '/') + ".class"
      getParent.getResourceAsStream(resourceName) match{
        case null => super.loadClass(resourceName)
        case is =>
          try
            val bytes = is.readAllBytes()
            val instrumentedBytes =
              if instrumentBytecode then ReplBytecodeInstrumentation.instrument(bytes)
              else bytes
            defineClass(name, instrumentedBytes, 0, instrumentedBytes.length)
          finally is.close()
      }
    catch
      case ex: Exception => super.loadClass(name)

  override def loadClass(name: String): Class[?] =
    if !instrumentBytecode then
      return super.loadClass(name)

    // Check if already loaded
    val loaded = findLoadedClass(name)
    if loaded != null then
      return loaded

    // Don't instrument JDK classes or StopRepl
    name match{
      case s"java.$_" => super.loadClass(name)
      case s"javax.$_" => super.loadClass(name)
      case s"sun.$_" => super.loadClass(name)
      case s"jdk.$_" => super.loadClass(name)
      case "dotty.tools.repl.StopRepl" =>
        // Load StopRepl from parent but ensure each classloader gets its own copy
        val is = getParent.getResourceAsStream(name.replace('.', '/') + ".class")
        if is != null then
          try
            val bytes = is.readAllBytes()
            defineClass(name, bytes, 0, bytes.length)
          finally
            is.close()
        else
          // Can't get as resource, use the classloader that loaded this AbstractFileClassLoader
          // class itself, which must have access to StopRepl
          classOf[AbstractFileClassLoader].getClassLoader.loadClass(name)
      case _ =>
        try findClass(name)
        catch case _: ClassNotFoundException =>
          // Not in REPL output, try to load from parent and instrument it
          tryInstrumentLibraryClass(name)
    }

end AbstractFileClassLoader
