// Not allowed due to name clash

inline trait A(var x: Int):
  val y = x

class C(x: Int) extends A(10): // error: Inlining of inline trait created name conflict on x. Constructor parameters of inline receivers may not collide with members of inline traits.
  val z = x

@main def Test = 
  val v = C(5)
  assert(v.y == 10)
  assert(v.x == 10)
  assert(v.z == 5)
  println(v.y)
  println(v.x)
