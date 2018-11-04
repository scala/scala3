class Test {
  type A

  class Inner {
    type B
    type Balias = B
    type Aalias = A
    type Intalias = Int
  }

  type T <: Inner
  type U = Inner

  def x0: T#B = ???        // error
  def x1: T#Balias = ???   // error
  def x2: T#Aalias = ???   // error
  def x3: T#Intalias = ??? // error
  def y0: U#B = ???        // ok
  def y1: U#Balias = ???   // ok
  def y2: U#Aalias = ???   // ok
  def y3: U#Intalias = ??? // ok
  def z0: Inner#B = ???        // ok
  def z1: Inner#Balias = ???   // ok
  def z2: Inner#Aalias = ???   // ok
  def z3: Inner#Intalias = ??? // ok
}