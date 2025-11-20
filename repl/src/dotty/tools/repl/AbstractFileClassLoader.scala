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

import java.net.{URL, URLConnection, URLStreamHandler}
import java.util.Collections

class AbstractFileClassLoader(val root: AbstractFile, parent: ClassLoader, interruptInstrumentation: String) extends ClassLoader(parent):
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
      if (file == null) throw new ClassNotFoundException(name)
    }
    file = file.lookupName(pathParts.last+".class", false)
    if (file == null) throw new ClassNotFoundException(name)

    val bytes = file.toByteArray

    if interruptInstrumentation != "false" then defineClassInstrumented(name, bytes)
    else defineClass(name, bytes, 0, bytes.length)
  }

  def defineClassInstrumented(name: String, originalBytes: Array[Byte]) = {
    val instrumentedBytes = ReplBytecodeInstrumentation.instrument(originalBytes)
    defineClass(name, instrumentedBytes, 0, instrumentedBytes.length)
  }

  override def loadClass(name: String): Class[?] =
    if interruptInstrumentation == "false" || interruptInstrumentation == "local"
    then return super.loadClass(name)

    val loaded = findLoadedClass(name) // Check if already loaded
    if loaded != null then return loaded

    name match { 
      // Don't instrument JDK classes or StopRepl. These are often restricted to load from a single classloader
      // due to the JDK module system, and so instrumenting them and loading the modified copy of the class
      // results in runtime exceptions
      case s"java.$_" => super.loadClass(name)
      case s"javax.$_" => super.loadClass(name)
      case s"sun.$_" => super.loadClass(name)
      case s"jdk.$_" => super.loadClass(name)
      case s"org.xml.sax.$_" => super.loadClass(name) // XML SAX API (part of java.xml module)
      case s"org.w3c.dom.$_" => super.loadClass(name) // W3C DOM API (part of java.xml module)
      case s"com.sun.org.apache.$_" => super.loadClass(name) // Internal Xerces implementation
      // Don't instrument StopRepl, which would otherwise cause infinite recursion
      case "dotty.tools.repl.StopRepl" =>
        // Load StopRepl bytecode from parent but ensure each classloader gets its own copy
        val classFileName = name.replace('.', '/') + ".class"
        val is = Option(getParent.getResourceAsStream(classFileName))
          // Can't get as resource, use the classloader that loaded this AbstractFileClassLoader
          // class itself, which must have access to StopRepl
          .getOrElse(classOf[AbstractFileClassLoader].getClassLoader.getResourceAsStream(classFileName))

        try
          val bytes = is.readAllBytes()
          defineClass(name, bytes, 0, bytes.length)
        finally Option(is).foreach(_.close())

      case _ =>
        try findClass(name)
        catch case _: ClassNotFoundException =>
          // Not in REPL output, try to load from parent and instrument it
          try
            val resourceName = name.replace('.', '/') + ".class"
            getParent.getResourceAsStream(resourceName) match {
              case null => super.loadClass(resourceName)
              case is =>
                try defineClassInstrumented(name, is.readAllBytes())
                finally is.close()
            }
          catch
            case ex: Exception => super.loadClass(name)
    }

end AbstractFileClassLoader
