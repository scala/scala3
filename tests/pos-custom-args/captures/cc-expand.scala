object Test:

  class A
  class B
  class C
  class CTC
  type CT = CTC retains *

  def test[X <: {*} Any, Y <: {*} Any](ct: CT, dt: CT) =

    def x0: A => {ct} B = ???

    def x1: A => B retains ct.type = ???
    def x2: A => B => C retains ct.type = ???
    def x3: A => () => B => C retains ct.type = ???

    def x4: (x: A retains ct.type) => B => C = ???

    def x5: A => (x: B retains ct.type) => () => C retains dt.type = ???
    def x6: A => (x: B retains ct.type) => (() => C retains dt.type) retains x.type | dt.type = ???
    def x7: A => (x: B retains ct.type) => (() => C retains dt.type) retains x.type = ???

    def x8: X => Y = ???
    def x9: {} X => Y = ???
