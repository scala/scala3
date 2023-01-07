@main def Test: Unit =
  println(typeMembers[A])
  println(typeMembers[B])
  println(declaredTypes[A])
  println(declaredTypes[B])

class A:
  type X

class B extends A:
  type Y
  type Z
