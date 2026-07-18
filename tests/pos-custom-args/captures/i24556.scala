import language.experimental.captureChecking

trait Item

trait ItOps[+T, +CC[_], +C]:
  def ++[B >: T](other: It[B]^): CC[B]^{this, other}

trait It[+T] extends ItOps[T, It, It[T]]

trait Sq[+T] extends It[T] with ItOps[T, Seq, Seq[T]]

def items: Sq[Item] = ???

@main def main(): It[Item] =
  items
  ++ items
  ++ items
  ++ items
  ++ items
  ++ items
  ++ items
  ++ items
  ++ items
  ++ items
  ++ items
  ++ items
  ++ items
  ++ items
  ++ items
  ++ items
  ++ items
  ++ items
  ++ items
  ++ items
  ++ items
