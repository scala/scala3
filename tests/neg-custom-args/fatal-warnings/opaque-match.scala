case class C()

object O:
  opaque type T <: C = C
  val x: T = C()
  (??? : Any) match
    case _: T => ??? // OK

def Test[T] =
  O.x match
    case _: C => ???   // ok
  C() match
    case _: O.T => ???  // error
  C() match
    case _: T => ???   // error

  (??? : Any) match
    case _: List[O.T] => ???  // error
  (??? : Any) match
    case _: List[O.T @unchecked] => ???  // OK 
  (??? : Any) match
    case _: List[T] => ???  // error




