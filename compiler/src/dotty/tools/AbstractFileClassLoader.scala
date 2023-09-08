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

import scala.language.unsafeNulls

import io.AbstractFile

import java.net.{URL, URLConnection, URLStreamHandler}
import java.util.Collections

class AbstractFileClassLoader(val root: AbstractFile, parent: ClassLoader) extends ClassLoader(parent):
  private def findAbstractFile(name: String) = root.lookupPath(name.split('/').toIndexedSeq, directory = false)

  // on JDK 20 the URL constructor we're using is deprecated,
  // but the recommended replacement, URL.of, doesn't exist on JDK 8
  @annotation.nowarn("cat=deprecation")
  override protected def findResource(name: String) =
    findAbstractFile(name) match
      case null => null
      case file => new URL(null, s"memory:${file.path}", new URLStreamHandler {
        override def openConnection(url: URL): URLConnection = new URLConnection(url) {
          override def connect() = ()
          override def getInputStream = file.input
        }
      })
  override protected def findResources(name: String) =
    findResource(name) match
      case null => Collections.enumeration(Collections.emptyList[URL])  //Collections.emptyEnumeration[URL]
      case url  => Collections.enumeration(Collections.singleton(url))

  override def findClass(name: String): Class[?] = {
    var file: AbstractFile = root
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
    val bytes = file.toByteArray
    defineClass(name, bytes, 0, bytes.length)
  }

  override def loadClass(name: String): Class[?] = try findClass(name) catch case _: ClassNotFoundException => super.loadClass(name)
end AbstractFileClassLoader
