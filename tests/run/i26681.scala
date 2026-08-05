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
  println(f(unstable.b))
  val o = unstable
  println(f(o.b))
