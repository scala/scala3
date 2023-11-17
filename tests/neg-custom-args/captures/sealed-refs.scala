class Ref[sealed A](init: A):
  this: Ref[A]^ =>
  private var x: A = init
  def get: A = x
  def set(x: A): Unit = this.x = x

class It[X]:
  this: It[X]^ =>

def f1[B1](x: B1, next: B1 -> B1) =
  var r = x    // ok
  r = next(x)
  r

def f2[B2](x: B2, next: B2 -> B2) =
  val r = Ref[B2](x)    // error
  r.set(next(x))
  r.get

def g[sealed B](x: B, next: B -> B) =
  val r = Ref[B](x)     // ok
  r.set(next(x))
  r.get

import annotation.unchecked.uncheckedCaptures

def h[B](x: B, next: B -> B) =
  val r = Ref[B @uncheckedCaptures](x)  // ok
  r.set(next(x))
  r.get

def f3[B](x: B, next: B -> B) =
  val r: Ref[B^{cap[f3]}] = Ref[B^{cap[f3]}](x)    // error
  r.set(next(x))
  val y = r.get
  ()

def f4[B](x: B, next: B -> B) =
  val r: Ref[B]^{cap[f4]} = Ref[B](x)    // error
  r.set(next(x))
  val y = r.get
  ()