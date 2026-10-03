// https://github.com/scala/scala3/issues/26681
class Box[T]

class Owner:
  val w: Int = 8
  val b: Box[w.type] = new Box[w.type]

def use[T](b: Box[T]): String = "ok"

def unstable: Owner = new Owner

transparent inline def f(inline rhs: Any): Any =
  inline rhs match
    case r: Box[t] => use[t](r)

@main def Test =
  // `unstable.b` has an unstable prefix, so its type is `Owner#b`, whose info is
  // computed by skolemizing the prefix. Retyping the selection while inlining used
  // to create a second skolem and overwrite the denotation of `Owner#b`, so the type
  // `t` inferred when reducing the inline match no longer matched the scrutinee.
  println(f(unstable.b))
  val o = unstable
  println(f(o.b))
