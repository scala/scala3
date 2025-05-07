package scala // @preview is private[scala]

import scala.annotation.internal.preview

@preview
def f1() = ???

@preview()
def f2() = ???

@preview("not yet stable")
def f3() = ???

def g() =
  f1() // error
  f2() // error
  f3() // error
