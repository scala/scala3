//> using options -language:experimental.inlineTraits
//> using options -Werror -Wsafe-init

inline trait A(val x: Int):
  val y = x // We need to be careful not to warn on this when we inline it but leave the body in the parent, because in the child the access is fine as we inline the parameter value,
            // while the rhs of val y in the parent inline trait is going to be pruned out later so the illegal access won't be possible.
class C extends A(10)

@main def Test = 
  val v = C()
  println(v.y)
  println(v.x)
