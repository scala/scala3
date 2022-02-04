@annotation.capability class Cap

def eff(using Cap): Unit = ()

class A:
  val x: A = this

class B extends A: // error
  this: {*} B =>

class C(val f: () => Int) extends A // error

class A2

class B2 extends A2:  // ok
  this: {*} B2 =>

class C2(val f: () => Int) extends A2 // ok
