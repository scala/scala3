

package scala.runtime

import language.experimental.captureChecking

object test:
  type T = Pure

class Foo extends Object, Pure:
  val x: Pure = ???
  def foo() = ()

