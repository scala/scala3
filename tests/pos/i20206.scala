//> using options -experimental

import language.experimental.erasedDefinitions

erased trait A
trait B

def foo1: A ?=> B ?=> Nothing = ???
def foo2: (A, B) ?=> Nothing = ???
def foo3: (B, A) ?=> Nothing = ???

def bar: (A, B) ?=> Nothing =
  foo1
  foo2
  foo3
