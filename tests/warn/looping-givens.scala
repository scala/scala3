//> using options -source 3.4

class A
class B

given joint(using a: A, b: B): (A & B) = ???

def foo(using a: A, b: B) =
  given aa: A = summon       // warn
  given bb: B = summon       // warn
  given ab: (A & B) = summon // warn
