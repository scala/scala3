lazy enum LazyList[+A] {  // error: sealed abstract types cannot be lazy enum
  case :: (head: A, tail: LazyList[A])
  case Nil
}