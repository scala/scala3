// Example 4 from OOPSLA 26

enum MyList[T]:
  case MyNil()
  case MyCons(head: T, tail: MyList[T])

import MyList.*

@annotation.tailrec
def collect[T, S >: T](
  xs: MyList[T],
  p: S => Boolean,
  acc: MyList[{v: T with p(v)}]
): MyList[{v: T with p(v)}] =
  xs match
    case MyNil() => acc
    case MyCons(x, xs1) =>
      if p(x) then collect(xs1, p, MyCons(x, acc))
      else collect(xs1, p, acc)
