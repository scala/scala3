//> using options -Werror -Wsafe-init

inline trait A(val x: Int):
  val y = x // We need to be careful not to warn on this when we inline it but leave the body in the parent. Pruning early enough avoids this.
class C extends A(10)

@main def Test = 
  val v = C()
  println(v.y)
  println(v.x)
