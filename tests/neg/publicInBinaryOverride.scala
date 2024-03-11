//> using options -experimental -Yno-experimental

import scala.annotation.publicInBinary

class A:
  @publicInBinary def f(): Unit = ()
  @publicInBinary def g(): Unit = ()

class B extends A:
  override def f(): Unit = () // error
  @publicInBinary override def g(): Unit = ()
