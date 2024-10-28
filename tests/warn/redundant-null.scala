class A
class B
class C

val n = null

def f(s: A) = s match
  case _: n.type =>
  case _: A =>
  case _: n.type => // warn
  case null =>
  case _ => // warn
  case _ => // warn

def f2(s: A | B | C) = s match
  case _: A => 0
  case _: C | null | _: B => 1
  case _ => 3 // warn

def f3(s: A | B) = s match
  case _: A =>
  case _ =>
  case _: B => // warn
  case _ => // warn
  case null => // warn

def f4(s: String | Int) = s match
  case _: Int =>
  case _: String =>
  case _ => // warn
  case null => // warn
  case _ => // warn
  case _ => // warn

def f5(x: String) = x match
  case x => println("catch all")
  case _ => println("unreachable") // warn

def test(s: String | Null) = s match    
  case ss => 
  case _ => // warn