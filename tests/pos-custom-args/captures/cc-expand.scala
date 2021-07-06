object Test:

  class A
  class B
  class C
  class CTC
  type CT = CTC retains *

  def test[X <: {*} Any, Y <: {*} Any](ct: CT, dt: CT) =

    def x0: A => {ct} B = ???

    def x1: A => B retains ct.type = ???
    def x2: A => B => {ct} C = ???
    def x3: A => () => B => {ct} C = ???

    def x4: (x: {ct} A) => B => C = ???

    def x5: A => (x: {ct} B) => () => {dt} C = ???
    def x6: A => (x: {ct} B) => {x, dt} (() => {dt} C) = ???
    def x7: A => (x: {ct} B) => {x} () => {dt} C = ???

    def x8: X => Y = ???
    def x9: {} X => Y = ???
