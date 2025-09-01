class A
class B
class C

val n = null

def f(s: A) = s match
  case _: n.type =>
  case _: A =>
  case _: n.type => // warn: unreachable
  case null =>
  case _ => // warn: unreachable
  case _ => // warn: unreachable

def f2(s: A | B | C) = s match
  case _: A => 0
  case _: C | null | _: B => 1
  case _ => 3 // warn: unreachable

def f3(s: A | B) = s match
  case _: A =>
  case _ =>
  case _: B => // warn: unreachable
  case _ => // warn: unreachable
  case null => // warn: unreachable

def f4(s: String | Int) = s match
  case _: Int =>
  case _: String =>
  case _ => // warn: null only
  case null => // warn: unreachable
  case _ => // warn: unreachable
  case _ => // warn: unreachable

def f5(x: String) = x match
  case x =>
  case _ => // warn: unreachable

def test(s: String | Null) = s match    
  case ss => 
  case _ => // warn: unreachable

def test2(s: String | Null) = s match    
  case ss: String => 
  case _ => // warn: null only