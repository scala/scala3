type HEAD[X <: NonEmptyTuple] = X match {
  case x *: (? <: NonEmptyTuple) => x
}

inline def head[A <: NonEmptyTuple](x: A): HEAD[A] = null.asInstanceOf[HEAD[A]]

def show[A, T <: Tuple](x: A *: T) =
  show1(head(x))
  show1(x.head)
def show1[A](x: A): String = ???