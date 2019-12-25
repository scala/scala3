lazy enum LazyList[+A] {  // error
  case :: (head: A, tail: LazyList[A])
  case Nil
}