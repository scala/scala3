
class ann(xs: Any) extends annotation.StaticAnnotation

class C

class D:
  def m: C = ???

trait Ops extends Any {
  def m: C @ann(this)
}

class Ops1(s: String) extends AnyVal with Ops:
  def a = new D
  def m = a.m
