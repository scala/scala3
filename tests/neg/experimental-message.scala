

import scala.annotation.experimental

@experimental
def f1() = ???

@experimental()
def f2() = ???

@experimental("not yet stable")
def f3() = ???

def g() =
  f1() // error
  f2() // error
  f3() // error
