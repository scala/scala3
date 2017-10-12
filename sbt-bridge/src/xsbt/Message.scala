/* sbt -- Simple Build Tool
 * Copyright 2008, 2009 Mark Harrah
 */
package xsbt

import java.util.function.Supplier

object Message {
  def apply[T](s: => T) = new Supplier[T] { def get() = s }
}
