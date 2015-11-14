package dotty.tools
package io

import java.io.{ InputStream }
import java.util.jar.JarEntry
import dotty.tools.dotc.core.Definitions
import language.postfixOps
import dotty.tools.dotc.core.Contexts._


/** A test to trigger issue with separate compilation between Dotty and Scalac and lazy vals */
object Foo {
  val definitions: Definitions = null
  def defn = definitions
  def go = defn.FunctionType(0)
}
