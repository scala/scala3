

package scala.runtime

import language.experimental.captureChecking

object test:
  type T = caps.Pure

class Foo extends Object, caps.Pure:
  val x: caps.Pure = ???
  def foo() = ()

