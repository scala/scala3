trait Tag[T] { type X }
class IntTag extends Tag[Int]

def foo[T](x: Tag[T]): T = x match {
  case _: IntTag { type X = String } => 0
}
