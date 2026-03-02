

import annotation.experimental
type A[T] = Int
class Foo {
  @experimental type Bar = (Int, String)
}

type Elem1[X <: Tuple, N <: Int] = X match { case x *: xs => N match { case 0 => x } }
type Elem2[X <: Tuple, N <: Int]

val f: Foo = Foo()

def bar1: f.Bar = ??? // error
def bar2 = // error
  ??? : f.Bar // error

def g0: Elem1[f.Bar, 0] = ??? // error
def g1(a: Elem1[f.Bar, 0]) = ??? // error
def g2 =
  ??? : Elem1[f.Bar, 0] // error

def h: Elem2[f.Bar, 0] = ??? // error
