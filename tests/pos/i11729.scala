//> using options -Werror -deprecation -feature

type Return[X] = X match
    case List[t] => List[t]
    case Any => List[X]

object Return:
  def apply[A](a:A):Return[A] = a match
    case a: List[?] => a
    case a: Any => List(a)

object Test1:
  Return(1).map(x => x)


type Boxed[X] = X match
   case Box[t] => Box[t]
   case Any => Box[X]

def box[X](x: X): Boxed[X] = x match
   case b: Box[?] => b
   case x: Any => Box(x)

case class Box[A](a:A):
  def map[B](f: A => B): Box[B] = Box(f(a))

object Test2:
   box(box(1)).map(_ + 1)
