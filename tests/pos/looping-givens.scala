
class A
class B

given joint(using a: A, b: B): (A & B) = ???

def foo(using a: A, b: B) =
  given aa: A = summon       // resolves to a
  given bb: B = summon       // resolves to b
  given ab: (A & B) = summon // resolves to joint(aa, bb)
